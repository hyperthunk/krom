package org.nebularis.krom

import org.nebularis.krom.*
import org.semanticweb.owlapi.model.IRI

import java.io.FileInputStream
import java.net.URL

@main
def main(): Unit =
    // TODO: we don't actually need a MAIN - convert to test cases
    val path = "https://raw.githubusercontent.com/hyperthunk/mork/refs/heads/main/spec/Mork.owl";
    val ont = Ontology.openOntology(IRI.create(new URL(path)))
    val src = new FileInputStream("src/test/resources/input.json")
    try {
        JsonLoader.load(src, ont)
        ont.print()
        ont.save()
    } finally {
        src.close()
    }