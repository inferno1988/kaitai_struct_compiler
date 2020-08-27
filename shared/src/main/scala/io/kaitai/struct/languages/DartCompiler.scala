package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.DartTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

class DartCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile
    with UpperCamelCaseClasses
    with UniversalFooter
    with ObjectOrientedLanguage
    with EveryReadIsExpression
    with AllocateIOLocalVar
    with NoNeedForFullClassPath {

  import DartCompiler._

  override val translator = new DartTranslator(typeProvider, importList)

  override def indent: String = "  "

  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.dart"

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    out.puts
  }

  override def classHeader(name: String): Unit = {
    out.puts(s"class ${type2class(name)} extends $kstructName {")
    out.inc
  }

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def runRead(name: List[String]): Unit = out.puts("_read();")

  override def runReadCalc(): Unit = {
    out.puts
    out.puts("if (_is_le == null) {")
    out.inc
    out.puts(s"throw new $kstreamName.UndecidedEndiannessError();")
    out.dec
    out.puts("} else if (_is_le) {")
    out.inc
    out.puts("_readLE();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts("_readBE();")
    out.dec
    out.puts("}")
  }


  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val suffix = endian match {
      case Some(e) => Utils.upperUnderscoreCase(e.toSuffix)
      case None => ""
    }
    out.puts(s"void _read$suffix() {")
    out.inc
  }

  override def readFooter(): Unit = universalFooter

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"private ${kaitaiType2DartType(attrType)} ${idToStr(attrName)};")
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"${kaitaiType2DartType(attrType)} ${idToStr(attrName)}() { return ${idToStr(attrName)}; }")

  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if (_is_le) {")
    out.inc
    leProc()
    out.dec
    out.puts("} else {")
    out.inc
    beProc()
    out.dec
    out.puts("}")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit = {
    out.puts(s"${privateMemberName(attrName)} = $normalIO.ensureFixedContents($contents);")
  }

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw): Unit = {
    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new List<List<int>>();")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = new List<List<int>>();")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2DartType(ArrayTypeInStream(dataType))}();")
    out.puts("{")
    out.inc
    out.puts("int i = 0;")
    out.puts(s"while (!$io.isEof()) {")
    out.inc
  }


  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, repeatExpr: Ast.expr): Unit = {
    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>(((Number) (${expression(repeatExpr)})).intValue());")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = new ArrayList<byte[]>(((Number) (${expression(repeatExpr)})).intValue());")
    out.puts(s"${idToStr(id)} = new ${kaitaiType2DartType(ArrayTypeInStream(dataType))}(((Number) (${expression(repeatExpr)})).intValue());")
    out.puts(s"for (int i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, repeatExpr: Ast.expr): Unit = {
    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>();")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = new ArrayList<byte[]>();")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2DartType(ArrayTypeInStream(dataType))}();")
    out.puts("{")
    out.inc
    out.puts(s"${kaitaiType2DartType(dataType)} ${translator.doName("_")};")
    out.puts("int i = 0;")
    out.puts("do {")
    out.inc
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, repeatExpr: Ast.expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts("i++;")
    out.dec
    out.puts(s"} while (!(${expression(repeatExpr)}));")
    out.dec
    out.puts("}")
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = idToStr(varName)
    rep match {
      case NoRepeat => memberName
      case _ => s"$memberName.get($memberName.size() - 1)"
    }
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        s"$kstreamName.processXor($srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        s"$kstreamName.processZlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName.processRotateLeft($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val namespace = name.init.mkString(".")
        val procClass = namespace +
          (if (namespace.nonEmpty) "." else "") +
          type2class(name.last)
        val procName = s"_process_${idToStr(varSrc)}"
        out.puts(s"$procClass $procName = new $procClass(${args.map(expression).mkString(", ")});")
        s"$procName.decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }


  override def useIO(ioEx: Ast.expr): String = {
    out.puts(s"$kstreamName io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"long _pos = $io.pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos);")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.alignToByte();")

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if (${privateMemberName(instName)} != null)")
    out.inc
    instanceReturn(instName, dataType)
    out.dec
  }


  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  /**
   * Renders identifier to a string, specifically for a given
   * language and settings. This usually includes things like
   * case and separator conversion and does *not* include things
   * like prepending "@" or "this." or "self." that might be
   * used to access private member.
   *
   * @param id identifier to render
   * @return identifier as string
   */
  override def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
    }
  }


  /**
   * Renders identifier as a proper reference to a private member
   * that represents this field. This might include some prefixes
   * like "@" or "this." or "self.".
   *
   * @param id identifier to render
   * @return identifier as string
   */
  override def privateMemberName(id: Identifier): String = s"this.${idToStr(id)}"

  /**
   * Renders identifier as a proper reference to a public member
   * that represents this field.
   *
   * @param id identifier to render
   * @return identifier as string
   */
  override def publicMemberName(id: Identifier): String = idToStr(id)

  /**
   * Renders identifier as a proper reference to a local temporary
   * variable appropriately named to hold a temporary reference to
   * this field.
   *
   * @param id identifier to render
   * @return identifier as string
   */
  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.add($expr);")
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.add($expr);")
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val (typeDecl, tempVar) = if (isRaw) {
      ("byte[] ", translator.doName(Identifier.ITERATOR2))
    } else {
      ("", translator.doName(Identifier.ITERATOR))
    }
    out.puts(s"$typeDecl$tempVar = $expr;")
    out.puts(s"${privateMemberName(id)}.add($tempVar);")
  }


  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = out.puts(s"${privateMemberName(id)} = $expr;")

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    val expr = dataType match {
      case t: ReadableType =>
        s"$io.read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.readBytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.readBytesTerm($terminator, $include, $consume, $eosError)"
      case BitsType1(bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width)"
      case t: UserType =>
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "null"
            case Some(fp) => translator.translate(fp)
            case None => "this"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => ", _is_le"
            case _ => ""
          }
          s", $parent, _root$addEndian"
        }
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), ", ", ", ", "")
        s"new ${types2class(t.name)}($io$addArgs$addParams)"
    }

    if (assignType != dataType) {
      s"(${kaitaiType2DartType(assignType)}) ($expr)"
    } else {
      expr
    }
  }


  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean): String = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.bytesStripRight($expr0, (byte) $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName.bytesTerminate($expr1, (byte) $term, $include)"
      case None => expr1
    }
    expr2
  }


  override def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        out.puts("private Boolean _is_le;")
      case _ =>
      // no _is_le variable
    }

    val paramsArg = Utils.join(params.map((p) =>
      s"${kaitaiType2DartType(p.dataType)} ${paramName(p.id)}"
    ), ", ", ", ", "")

    if (isHybrid) {
      // Inherited endian classes can be only internal, so they have mandatory 4th argument
      // and 1..3-argument constructors don't make sense

      out.puts
      out.puts(s"public ${type2class(name)}($kstreamName _io, ${kaitaiType2DartType(parentType)} _parent, ${type2class(rootClassName)} _root, boolean _is_le$paramsArg) {")
      out.inc
      out.puts("super(_io);")
      out.puts("this._parent = _parent;")
      out.puts("this._root = _root;")
      out.puts("this._is_le = _is_le;")
    } else {
      // Normal 3 constructors, chained into the last

      val paramsRelay = Utils.join(params.map((p) => paramName(p.id)), ", ", ", ", "")

      out.puts
      out.puts(s"public ${type2class(name)}($kstreamName _io$paramsArg) {")
      out.inc
      out.puts(s"this(_io, null, null$paramsRelay);")
      out.dec
      out.puts("}")

      out.puts
      out.puts(s"public ${type2class(name)}($kstreamName _io, ${kaitaiType2DartType(parentType)} _parent$paramsArg) {")
      out.inc
      out.puts(s"this(_io, _parent, null$paramsRelay);")
      out.dec
      out.puts("}")

      out.puts
      out.puts(s"public ${type2class(name)}($kstreamName _io, ${kaitaiType2DartType(parentType)} _parent, ${type2class(rootClassName)} _root$paramsArg) {")
      out.inc
      out.puts("super(_io);")
      out.puts("this._parent = _parent;")
      if (name == rootClassName) {
        out.puts("this._root = _root == null ? this : _root;")
      } else {
        out.puts("this._root = _root;")
      }
    }

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))
  }


  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"public ${kaitaiType2DartType(dataType)} ${idToStr(instName)}() {")
    out.inc
  }

  def value2Const(s: String): String = Utils.upperUnderscoreCase(s)

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    val enumClass = type2class(enumName)

    out.puts
    out.puts(s"public enum $enumClass {")
    out.inc

    if (enumColl.size > 1) {
      enumColl.dropRight(1).foreach { case (id, label) =>
        out.puts(s"${value2Const(label)}(${translator.doIntLiteral(id)}),")
      }
    }
    enumColl.last match {
      case (id, label) =>
        out.puts(s"${value2Const(label)}(${translator.doIntLiteral(id)});")
    }

    out.puts
    out.puts("private final long id;")
    out.puts(s"$enumClass(long id) { this.id = id; }")
    out.puts("public long id() { return id; }")
    out.puts(s"private static final Map<Long, $enumClass> byId = new HashMap<Long, $enumClass>(${enumColl.size});")
    out.puts("static {")
    out.inc
    out.puts(s"for ($enumClass e : $enumClass.values())")
    out.inc
    out.puts(s"byId.put(e.id(), e);")
    out.dec
    out.dec
    out.puts("}")
    out.puts(s"public static $enumClass byId(long id) { return byId.get(id); }")
    out.dec
    out.puts("}")
  }


  override def switchStart(id: Identifier, on: Ast.expr): Unit = out.puts(s"switch (${expression(on)}) {")

  override def switchCaseStart(condition: Ast.expr): Unit = {
    // Java is very specific about what can be used as "condition" in "case
    // condition:".
    val condStr = condition match {
      case enumByLabel: Ast.expr.EnumByLabel =>
        // If switch is over a enum, only literal enum values are supported,
        // and they must be written as "MEMBER", not "SomeEnum.MEMBER".
        value2Const(enumByLabel.label.name)
      case _ =>
        expression(condition)
    }

    out.puts(s"case $condStr: {")
    out.inc
  }


  override def switchCaseEnd(): Unit = {
    out.puts("break;")
    out.dec
    out.puts("}")
  }


  override def switchElseStart(): Unit = {
    out.puts("default: {")
    out.inc
  }

  override def switchEnd(): Unit = {
    out.puts("}")
  }

  /**
   * Resolves string name of exception in target language.
   * Suggested implementation is to use `err.name` that provides
   * UpperCamelCase renditions of original names.
   *
   * @param err KS-generated error that might be thrown in runtime
   * @return name of exception as a string
   */
  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => "EndOfStreamError"
    case _ => s"KaitaiStream.${err.name}"
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val dartName = idToStr(varName)

    val ioName = s"_io_$dartName"

    val args = rep match {
      case RepeatUntil(_) => translator.doName(Identifier.ITERATOR2)
      case _ => getRawIdExpr(varName, rep)
    }

    out.puts(s"$kstreamName $ioName = new ByteBufferKaitaiStream($args);")
    ioName
  }

}

object DartCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {
  override def getCompiler(tp: ClassTypeProvider, config: RuntimeConfig): LanguageCompiler = new DartCompiler(tp, config)

  override def kstreamName: String = "KaitaiStream"

  override def kstructName: String = "KaitaiStruct"

  def types2class(names: List[String]) = names.map(x => type2class(x)).mkString(".")

  def kaitaiType2DartType(attrType: DataType): String = kaitaiType2DartTypePrim(attrType)

  def kaitaiType2DartTypePrim(attrType: DataType): String = {
    attrType match {
      case Int1Type(false) => "int"
      case IntMultiType(false, Width2, _) => "int"
      case IntMultiType(false, Width4, _) => "int"
      case IntMultiType(false, Width8, _) => "int"

      case Int1Type(true) => "int"
      case IntMultiType(true, Width2, _) => "int"
      case IntMultiType(true, Width4, _) => "int"
      case IntMultiType(true, Width8, _) => "int"

      case FloatMultiType(Width4, _) => "double"
      case FloatMultiType(Width8, _) => "double"

      case BitsType(_, _) => "int"

      case _: BooleanType => "bool"
      case CalcIntType => "int"
      case CalcFloatType => "double"

      case _: StrType => "String"
      case _: BytesType => "List<int>"

      case AnyType => "dynamic"
      case KaitaiStreamType | OwnedKaitaiStreamType => kstreamName
      case KaitaiStructType | CalcKaitaiStructType => kstructName

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => types2class(name)

      case ArrayTypeInStream(inType) => s"ArrayList<${kaitaiType2DartTypePrim(inType)}>"
      case CalcArrayType(inType) => s"ArrayList<${kaitaiType2DartTypePrim(inType)}>"

      case st: SwitchType => kaitaiType2DartTypePrim(st.combinedType)
    }
  }

}

