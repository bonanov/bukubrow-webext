import { Eq } from 'fp-ts/lib/Eq';
import * as A from 'fp-ts/lib/Array';

export const lookupC = (i: number) => <T>(xs: T[]): Option<T> => A.lookup(i, xs);

/**
 * `fp-ts/lib/Array::snoc` that doesn't resolve as a `NonEmptyArray`.
 */
export const snoc_ = <T>(xs: T[]) => (y: T): T[] => xs.concat(y);

export const asArray = <A>(xs: A | Array<A>): Array<A> => Array.isArray(xs) ? xs : [xs];

export const elemC = <A>(eq: Eq<A>) => (x: A) => (ys: Array<A>): boolean => A.elem(eq)(x, ys);

export const join = (y: string) => (xs: Array<string>): string => xs.join(y);

export const consC = <A>(xs: Array<A>) => (y: A): NonEmptyArray<A> => A.cons(y, xs);

export const snocC = <A>(xs: Array<A>) => (y: A): NonEmptyArray<A> => A.snoc(xs, y);
