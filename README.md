# scala-markov-processor

A Markov processor implementation in Scala for generating random text based on word frequencies of a given corpus.

This library was written as a first dive into learning Scala, so no guarantees about code quality or efficiency are made.

## Example usage

    import scala.io.Source
    import com.evnm.markovprocessor._

    // Read a corpus of text from a file.
    val corpus = Source.fromFile("hamlet.txt").mkString

    // Generate and print roughly 100 words of random text, using an Ngram
    // length of 4.
    val random_text = MarkovProcessor.generateText(100, 4, corpus)
    println(random_text)
