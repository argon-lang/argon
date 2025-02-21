export function $xto_s$a$bbool$a$e$t$e$r$bstring$a$e(r0, r1) {
  let r2;
  let r3;
  if (r0) {
    r2 = "true";
    return r2;
  } else {
    r3 = "false";
    return r3;
  }
}
export function Bool$a$r$_() {
  let r0;
  r0 = globalThis.Boolean;
  return r0;
}
export function $ulogical_not$a$bbool$a$e$r$bbool$a$e(r0) {
  let r1;
  let r2;
  if (r0) {
    r1 = false;
    return r1;
  } else {
    r2 = true;
    return r2;
  }
}
export function $bnot_equal$a$bbool$a$e$bbool$a$e$r$bbool$a$e(r0, r1) {
  let r2;
  r2 = $bequal$a$bbool$a$e$bbool$a$e$r$bbool$a$e(r0, r1);
  return $ulogical_not$a$bbool$a$e$r$bbool$a$e(r2);
}
export function $bequal$a$bbool$a$e$bbool$a$e$r$bbool$a$e(r0, r1) {
  let r2;
  let r3;
  let r4;
  let r5;
  if (r0) {
    if (r1) {
      r2 = true;
      return r2;
    } else {
      r3 = false;
      return r3;
    }
  } else {
    if (r1) {
      r4 = false;
      return r4;
    } else {
      r5 = true;
      return r5;
    }
  }
}
