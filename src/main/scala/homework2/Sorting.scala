package homework2

import scala.annotation.tailrec

def quickSort[A](as: List[A])(isSmaller: (A, A) => Boolean): List[A] = as match
  case Nil => Nil
  case a :: rest =>
    val (smaller, bigger) = rest.partition(isSmaller(_, a))

    quickSort(smaller)(isSmaller) ::: (a :: quickSort(bigger)(isSmaller))

def mergeSort[A](as: List[A])(isSmaller: (A, A) => Boolean): List[A] =
  def merge(xs: List[A], ys: List[A]): List[A] = (xs, ys) match
    case (Nil, _) => ys
    case (_, Nil) => xs
    case (x :: xrest, y :: yrest) =>
      if isSmaller(x, y) then x :: merge(xrest, ys)
      else y :: merge(xs, yrest)

  if as.isEmpty || as.tail.isEmpty then as
  else
    val (left, right) = as.splitAt(as.size / 2)

    merge(
      mergeSort(left)(isSmaller),
      mergeSort(right)(isSmaller)
    )

def mergeSortWithTailRecMerge[A](as: List[A])(isSmaller: (A, A) => Boolean): List[A] =
  @tailrec
  def merge(xs: List[A], ys: List[A])(acc: List[A]): List[A] = (xs, ys) match
    case (x :: xrest, Nil) => merge(xrest, Nil)(x :: acc)
    case (Nil, y :: yrest) => merge(Nil, yrest)(y :: acc)
    case (x :: xrest, y :: yrest) =>
      if isSmaller(x, y) then merge(xrest, ys)(x :: acc)
      else merge(xs, yrest)(y :: acc)
    case _ => acc.reverse

  if as.isEmpty || as.tail.isEmpty then as
  else
    val (left, right) = as.splitAt(as.size / 2)

    merge(
      mergeSortWithTailRecMerge(left)(isSmaller),
      mergeSortWithTailRecMerge(right)(isSmaller)
    )(List.empty)
