package org.nebularis.krom

import org.nebularis.krom.*

@main
def main(): Unit =
    val config = KromConfig.load()
    val ont = JsonLoader.load(config)
    ont.print()
    ont.save()
