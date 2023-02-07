import cats.effect.*
import cats.implicits.*

import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.time.format.DateTimeFormatter

import scala.jdk.CollectionConverters.*

object CLI extends IOApp {
  private val imageExtensions = List(".jpg", ".jpeg", ".png", ".gif", ".bmp")

  override def run(args: List[String]): IO[ExitCode] = {
    if (args.length != 1) {
      IO(println("Please provide a folder path")).as(ExitCode.Error)
    } else {
      for {
        root <- IO(Paths.get(args.headOption.getOrElse(".")))
        target <- IO(root.resolve("by_date"))
        _ <- IO(Files.createDirectories(target))
        result <- traverseFolder(root, target).attempt
        exitCode <- result match {
          case Right(_) => IO.pure(ExitCode.Success)
          case Left(error) => IO(println(s"An error occurred: $error")).as(ExitCode.Error)
        }
      } yield exitCode
    }
  }

  private def traverseFolder(folder: Path, target: Path): IO[Unit] = for {
    files <- IO(folder.toFile.listFiles).attempt
    result <- files match {
      case Right(files) => files.toList.traverse(file => traverseFile(file.toPath, target))
      case Left(error) => IO.raiseError[Unit](error)
    }
  } yield result

  private def traverseFile(file: Path, target: Path): IO[Unit] = {
    if (Files.isDirectory(file)) traverseFolder(file, target)
    else moveImage(file, target)
  }

  private def isImage(path: Path): Boolean = {
    val fileName = path.getFileName.toString
    val extension = fileName.substring(fileName.lastIndexOf(".")).toLowerCase

    imageExtensions.contains(extension)
  }

  private def moveImage(image: Path, target: Path): IO[Unit] = {
    if (!isImage(image)) IO.unit

    else
      for {
        attributes <- IO(Files.readAttributes(image, "basic:creationTime")).attempt
        result <- attributes match {
          case Right(attrs) =>
            val scalaAttrs = attrs.asScala
            val creationTime = scalaAttrs("creationTime")
            val newFolder = target.resolve(creationTime.toString.substring(0, 10))

            for {
              _ <- IO(Files.createDirectories(newFolder)).attempt
              _ <- IO(Files.move(image, newFolder.resolve(image.getFileName))).attempt
            } yield ()

          case Left(error) => IO.raiseError(error)
        }
      } yield result
  }
}
