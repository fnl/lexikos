package fnl.collection

import scala.io.Source

/* lexikos/fnl.collection [2013-09-23T14:52]
 * (C) Florian Leitner 2013. All rights reserved. */

object LexiconBuilderApp extends App {
	def top = new MainFrame {
		title = "Lexicon App"
		val lexiconFileButton = new Button {
			text = "Lexicon file:"
		}
		val inputFileButton = new Button {
			text = "Input words/text:"
		}
		val lexiconFileName = new TextField()
		val inputFileName = new TextField()
		val lexiconErrors = new Label()
		val inputErrors = new Label()
		val generateLexiconButton = new Button {
			text = "Generate Lexicon"
		}
		val annotateTextButton = new Button {
			text = "Annotate Text"
		}
		val viewLexiconButton = new Button {
			text = "View Lexicon"
		}
		val fileChooser = new FileChooser(new File("./"))

		contents = new GridPanel(5, 2) {
			contents += inputFileButton
			contents += lexiconFileButton
			contents += inputFileName
			contents += lexiconFileName
			contents += inputErrors
			contents += lexiconErrors
			contents += generateLexiconButton
			contents += annotateTextButton
			contents += new Label()
			contents += viewLexiconButton
		}

		listenTo(inputFileButton, lexiconFileButton, generateLexiconButton, annotateTextButton)

		def selectFile(parent: Component, field: TextField, error: Label): String = {
			field.text = ""
			error.text = ""
			if (fileChooser.showOpenDialog(parent) == FileChooser.Result.Approve)
				fileChooser.selectedFile.getCanonicalPath
			else { error.text = "ERROR"; "" }
		}

		def generateLexicon() {
			inputErrors.text = ""
			lexiconErrors.text = ""
			val inputFile = new File(inputFileName.text)
			val lexiconFile = new File(lexiconFileName.text)
			if (!inputFile.canRead) inputErrors.text = "cannot read file"
			if (!lexiconFile.getParentFile.canWrite) lexiconErrors.text = "cannot write to dir"
			if (lexiconFile.exists && !lexiconFile.canWrite) lexiconErrors.text = "cannot overwrite file"
			if (inputFile.canRead && lexiconErrors.text.equals("")) {
				try {
					val out = new ObjectOutputStream(new FileOutputStream(lexiconFile))
					val in = Source.fromFile(inputFileName.text).getLines()
					out.writeObject(Lexicon.fromIterator[Char](in.map(_.toList)))
					out.close()
				} catch {
					case e: IOException => lexiconErrors.text = e.getMessage
					case e: AssertionError => inputErrors.text = "words not sorted"
				}
			}
		}

		def annotateText() {
			inputErrors.text = "ERROR:"
			lexiconErrors.text = "not implemented"
		}

		def viewLexicon() {
			lexiconErrors.text = ""
			val lexiconFile = new File(lexiconFileName.text)
			if (lexiconFile.canRead) {
				try {
					val in = new ObjectInputStream(new FileInputStream(lexiconFile))
					val lexicon = in.readObject.asInstanceOf[Lexicon[Char]]
					in.close()

				} catch {
					case e: IOException => lexiconErrors.text = e.getMessage
				}
			} else lexiconErrors.text = "cannot read file"
		}

		reactions += {
			case ButtonClicked(`inputFileButton`) =>
				inputFileName.text = selectFile(inputFileButton, inputFileName, inputErrors)
			case ButtonClicked(`lexiconFileButton`) =>
				lexiconFileName.text = selectFile(lexiconFileButton, lexiconFileName, lexiconErrors)
			case ButtonClicked(`generateLexiconButton`) => generateLexicon()
			case ButtonClicked(`annotateTextButton`) => annotateText()
		}
	}
}
