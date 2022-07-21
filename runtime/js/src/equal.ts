
export function isEqual(a: any, b: any): boolean {
    if(a === b) {
        return true;
    }
    else if("canEqual" in b && "equals" in a) {
        return b.canEqual(a) && a.equals(b);
    }
    else {
        return false;
    }
}

export function arrayEqual<A>(x: readonly A[], y: readonly A[]): boolean {
    if(x.length != y.length) {
        return false;
    }

    for(let i = 0; i < x.length; ++i) {
        if(!isEqual(x[i], y[i])) {
            return false;
        }
    }

    return true;
}
