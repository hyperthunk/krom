package org.nebularis.krom

import OntologyFormat.{OwlXml, Turtle}

import com.typesafe.config.{Config, ConfigFactory}
import org.semanticweb.owlapi.formats.{OWLXMLDocumentFormat, TurtleDocumentFormat}
import org.semanticweb.owlapi.model.{IRI, OWLDocumentFormat}

import java.io.*
import java.net.{URI, URL, URLConnection}

/** Base exception for throwing parsing failures as RuntimeExceptions.
 */
class ConfigurationException(msg: String, cause: Throwable) extends RuntimeException(msg, cause) {
    def this(msg: String) = this(msg, null)
}

enum OntologyFormat:
    case Turtle extends OntologyFormat
    case OwlXml extends OntologyFormat

private case class IOMode(filePath: Option[String])

class KromConfig private(val morkURL: URL,
                         val morkBaseIRI: String,
                         val skosBaseIRI: String,
                         val kromBaseIRI: String,
                         val input: IOMode,
                         val output: IOMode,
                         val format: OntologyFormat) {

    def outputFormat: OWLDocumentFormat = format match {
        case Turtle => TurtleDocumentFormat()
        case OwlXml => OWLXMLDocumentFormat()
    }

    def withInputStream(fn: InputStream => Ontology): Ontology = input match {
        case IOMode(None) => fn(System.in)
        case IOMode(Some(path)) =>
            val io = FileInputStream(path)
            try fn(io)
            finally io.close()
    }

    def withOutputStream(fn: OutputStream => Ontology): Ontology = output match {
        case IOMode(None) => fn(System.out)
        case IOMode(Some(path)) =>
            val io = prepareActualOutput(IRI.create(path))
            try fn(io)
            finally io.close()
    }

    def prepareActualOutput(documentIRI: IRI): OutputStream = {
        // TODO: leverage SystemOutDocumentTarget in owlapi
        if ("file" == documentIRI.getScheme) {
            val file = new File(documentIRI.toURI)
            file.getParentFile.mkdirs
            FileOutputStream(file)
        } else {
            val url: URL = documentIRI.toURI.toURL
            val conn: URLConnection = url.openConnection
            conn.getOutputStream
        }
    }
}

object KromConfig {
    private final val baseConfigPath = "org.nebularis.krom."
    private final val morkOntologyUrlKey: String = key("MorkOntologyUrl")
    private final val inputJsonFileKey: String = key("InputJsonUrl")
    private final val outputOntologyFileKey: String = key("OutputOntologyFile")
    private final val inputStdIOKey: String = key("InputStdIO")
    private final val outputStdIOKey: String = key("OutputStdIO")
    private final val outputOntologyFormatKey: String = key("OutputOntologyFormat")
    private final val skosBaseIRIKey: String = key("SkosBaseIRI")
    private final val morkBaseIRIKey: String = key("MorkBaseIRI")
    private final val kromBaseIRIKey: String = key("KromBaseIRI")

    def key(s: String): String = baseConfigPath + s

    def load(): KromConfig =
        try
            val config = ConfigFactory.load()
            val morkUrl = config.getString(morkOntologyUrlKey) // set by default
            val inputIO = configIO(config, inputJsonFileKey, inputStdIOKey)
            val outputIO = configIO(config, outputOntologyFileKey, outputStdIOKey)
            val format = config.getString(outputOntologyFormatKey)
            val skos = config.getString(skosBaseIRIKey)
            val mork = config.getString(morkBaseIRIKey)
            val krom = config.getString(kromBaseIRIKey)
            KromConfig(URI.create(morkUrl).toURL, mork, skos, krom,
                inputIO, outputIO, OntologyFormat.valueOf(format))
        catch
            case cfgEx: ConfigurationException => throw new ConfigurationException(cfgEx.getMessage, cfgEx)

    private def configIO(conf: Config, key: String, sio: String): IOMode =
        if conf.hasPath(key) then IOMode(Some(conf.getString(key)))
        else if conf.hasPath(sio) && conf.getString(sio).eq("Y") then IOMode(None)
        else throw new ConfigurationException("Unable to load from stdio or " + key)

}
