/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import scala.util.{ Failure, Success, Try }

import play.api.libs.functional.Functor

import JsResult.functorJsResult

import org.scalatest._
import org.scalacheck.Gen

class JsResultSpec extends WordSpec
    with MustMatchers with org.scalatest.prop.PropertyChecks {

  "JSON Result" should {
    "be functor" in {
      val jsres = JsSuccess("jsStr")

      implicitly[Functor[JsResult]].
        fmap[String, List[Char]](jsres, _.toList).mustEqual(
          JsSuccess(List('j', 's', 'S', 't', 'r'))
        )
    }

    "be converted to Success" in {
      JsResult.toTry(JsSuccess("foo")) mustEqual Success("foo")
    }

    "be converted to basic Failure" in {
      val err = JsError("bar")
      JsResult.toTry(err) mustEqual Failure(JsResult.Exception(err))
    }

    "be created from successful input" in forAll(
      Gen.oneOf[(Int, Try[Int], JsPath)](
        (1, Success(1), JsPath),
        (2, Try(2), (JsPath \ "foo"))
      )
    ) {
        case (value, succ, path) =>
          JsResult.fromTry(succ, path) mustEqual JsSuccess(value, path)
      }

    "be JsSuccess from unsafe input" in forAll(
      Gen.oneOf[(Int, (() => Int), JsPath)](
        (1, (() => 1), JsPath),
        (2, (() => 5 - 3), (JsPath \ "foo"))
      )
    ) {
        case (value, gen, path) =>
          JsResult(gen(), path) mustEqual JsSuccess(value, path)
      }

    "be created from failed input" in {
      @inline def fixture[T](path: JsPath, ex: Exception, g: Exception => Try[T]): (Exception, Try[T], JsPath) = (ex, g(ex), path)

      forAll(Gen.oneOf(
        fixture[Float](JsPath, new Exception("Foo"), Failure(_)),
        fixture[Float](
          JsPath \ "bar",
          new IllegalArgumentException("Lorem"),
          { ex => Try[Float](throw ex) }
        ))
      ) {
        case (ex, failure, path) =>
          JsResult.fromTry[Float](failure, path) mustEqual JsError(
            path -> JsonValidationError(ex.getMessage, ex))
      }
    }

    "be JsError from unsafe input" in {
      @inline def fixture[T](path: JsPath, ex: Exception): (Exception, (() => Int), JsPath) = (ex, (() => throw ex), path)

      forAll(Gen.oneOf(
        fixture[String](JsPath, new Exception("Bar")),
        fixture[String](JsPath \ "foo", new RuntimeException("Lorem"))
      )) {
        case (ex, gen, path) =>
          JsResult(gen(), path) mustEqual JsError(
            path -> JsonValidationError(ex.getMessage, ex))
      }
    }

    "handle exception when map'ing" in {
      val ex = new Exception("Foo")
      def fixture(path: JsPath, in: JsResult[Int]): (JsResult[String], JsResult[String]) = {
        in.map[String](_ => throw ex) -> JsError(path -> JsonValidationError(
          ex.getMessage, ex))
      }

      forAll(Gen.oneOf(
        fixture(JsPath, JsSuccess(1)),
        fixture(JsPath \ "bar", JsSuccess(2, JsPath \ "bar")),
        fixture(JsPath, JsError(
          JsPath, JsonValidationError(ex.getMessage, ex)))
      )) {
        case (in, expected) => in mustEqual expected
      }
    }
  }
}
