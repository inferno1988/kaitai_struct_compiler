package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast.expr

class JavaScriptTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def doName(s: String) = s"this.${Utils.lowerCamelCase(s)}"

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
    s"Number.parseInt(${translate(s)}, ${translate(base)})"

  override def strLength(s: expr): String =
    s"${translate(s)}.length"

  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.substring(${translate(from)}, ${translate(to)})"
}
