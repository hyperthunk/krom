package org.nebularis.krom

import org.nebularis.krom.*
import com.github.vickumar1981.stringdistance.StringDistance._
import com.github.vickumar1981.stringdistance.StringSound._
import com.github.vickumar1981.stringdistance.impl.{ConstantGap, LinearGap}
import com.github.vickumar1981.stringdistance.MetaphoneScore

@main
def main(): Unit =
    val metaphone: Boolean = Metaphone.score("merci", "mercy")
    println(s"Metaphone score: $metaphone")
    val config = KromConfig.load()
    val ont = JsonLoader.load(config)
    ont.print()
    ont.save()
