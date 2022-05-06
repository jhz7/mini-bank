package com.bank.http

import cats.data.{ Validated, ValidatedNel }

object Validation {
  // Type Classes (TC)
  /// field must be present
  trait Required[T]   extends (T => Boolean)
  /// minimum value
  trait Minimum[T]    extends ((T, Double) => Boolean)
  /// minimum abs value
  trait MinimumAbs[T] extends ((T, Double) => Boolean)

  // TCs instances
  implicit val requiredString: Required[String]             = _.nonEmpty
  implicit val minimumBigDecimal: Minimum[BigDecimal]       = _ >= _
  implicit val minimumAbsBigDecimal: MinimumAbs[BigDecimal] = _.abs >= _

  // TC usage
  def required[A](value: A)(implicit req: Required[A]): Boolean                        = req(value)
  def minimum[A](value: A, threshold: Double)(implicit min: Minimum[A]): Boolean       = min(value, threshold)
  def minimumAbs[A](value: A, threshold: Double)(implicit min: MinimumAbs[A]): Boolean = min(value, threshold)

  // Validated
  type ValidationResult[A] = ValidatedNel[ValidationFailure, A]

  trait ValidationFailure {
    def errorMessage: String
  }
  final case class EmptyField(fieldName: String)                     extends ValidationFailure {
    def errorMessage: String = s"$fieldName is empty"
  }
  final case class NegativeValue(fieldName: String)                  extends ValidationFailure {
    def errorMessage: String = s"$fieldName is negative"
  }
  final case class BelowMinimunValue(fieldName: String, min: Double) extends ValidationFailure {
    def errorMessage: String = s"$fieldName is below minimum threshold $min"
  }

  // "main" API
  def validateMinimum[A: Minimum](value: A, fieldName: String, threshold: Double): ValidationResult[A] =
    if (minimum(value, threshold))
      Validated.valid(value)
    else if (threshold == 0)
      Validated.invalidNel(NegativeValue(fieldName))
    else
      Validated.invalidNel(BelowMinimunValue(fieldName, threshold))

  def validateMinimumAbs[A: MinimumAbs](value: A, fieldName: String, threshold: Double): ValidationResult[A] =
    if (minimumAbs(value, threshold))
      Validated.valid(value)
    else
      Validated.invalidNel(BelowMinimunValue(fieldName, threshold))

  def validateRequired[A: Required](value: A, fieldName: String): ValidationResult[A] =
    if (required(value))
      Validated.valid(value)
    else
      Validated.invalidNel(EmptyField(fieldName))

  // general TC for requests
  trait Validator[A] extends (A => ValidationResult[A])

  def validateEntity[A](value: A)(implicit validator: Validator[A]): ValidationResult[A] =
    validator(value)
}
