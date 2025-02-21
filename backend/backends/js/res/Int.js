export function $bless_than$a$bint$a$e$bint$a$e$r$bbool$a$e(r0, r1) {
  let r2;
  r2 = r0 < r1;
  return r2;
}
export function $bbit_or$a$bint$a$e$bint$a$e$r$bint$a$e(r0, r1) {
  let r2;
  r2 = r0 | r1;
  return r2;
}
export function $bgreater_than_eq$a$bint$a$e$bint$a$e$r$bbool$a$e(r0, r1) {
  let r2;
  r2 = r0 >= r1;
  return r2;
}
export function $bmul$a$bint$a$e$bint$a$e$r$bint$a$e(r0, r1) {
  let r2;
  r2 = r0 * r1;
  return r2;
}
export function $bbit_xor$a$bint$a$e$bint$a$e$r$bint$a$e(r0, r1) {
  let r2;
  r2 = r0 ^ r1;
  return r2;
}
export function $bbit_and$a$bint$a$e$bint$a$e$r$bint$a$e(r0, r1) {
  let r2;
  r2 = r0 & r1;
  return r2;
}
export function $bnot_equal$a$bint$a$e$bint$a$e$r$bbool$a$e(r0, r1) {
  let r2;
  r2 = r0 !== r1;
  return r2;
}
export function $bshift_left$a$bint$a$e$bint$a$e$r$bint$a$e(r0, r1) {
  let r2;
  r2 = r0 << r1;
  return r2;
}
export function $uplus$a$bint$a$e$r$bint$a$e(r0) {
  return r0;
}
export function $bequal$a$bint$a$e$bint$a$e$r$bbool$a$e(r0, r1) {
  let r2;
  r2 = r0 === r1;
  return r2;
}
export function $bgreater_than$a$bint$a$e$bint$a$e$r$bbool$a$e(r0, r1) {
  let r2;
  r2 = r0 > r1;
  return r2;
}
export function $bminus$a$bint$a$e$bint$a$e$r$bint$a$e(r0, r1) {
  let r2;
  r2 = r0 - r1;
  return r2;
}
export function $ubit_not$a$bint$a$e$r$bint$a$e(r0) {
  let r1;
  r1 = ~r0;
  return r1;
}
export function $bless_than_eq$a$bint$a$e$bint$a$e$r$bbool$a$e(r0, r1) {
  let r2;
  r2 = r0 <= r1;
  return r2;
}
export const $xto_s$a$bint$a$e$t$e$r$bstring$a$e = function int_to_s(i) {
  return i.toString();
};
export function $bplus$a$bint$a$e$bint$a$e$r$bint$a$e(r0, r1) {
  let r2;
  r2 = r0 + r1;
  return r2;
}
export function $uminus$a$bint$a$e$r$bint$a$e(r0) {
  let r1;
  r1 = -r0;
  return r1;
}
export function $bshift_right$a$bint$a$e$bint$a$e$r$bint$a$e(r0, r1) {
  let r2;
  r2 = r0 >> r1;
  return r2;
}
export function Int$a$r$_() {
  let r0;
  r0 = globalThis.BigInt;
  return r0;
}
