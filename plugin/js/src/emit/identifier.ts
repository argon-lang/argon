import type * as tube from "@argon-lang/plugin-api/tube";


// Escape Sequences
// $a - argument to a type
// $b - prepended to an identifier if it is not a valid JS start character
// $c - indicates a class type will follow
// $d - indicates a tube name segment will follow
// $e - prepended to an extension identifier
// $f - indicates a function type will follow
// $g - indicates a module name segment will follow
// $i - prepended to an inverse identifier
// $m - prepended to an update identifier
// $n - indicates that a name will follow
// $o - prepended to an operator name
// $p - indicates a signature parameter type
// $r - indicates a result type
// $t - indicates a trait type will follow
// $uXXXX and $UXXXXXXXX - unicode escape
// $v - indicates a signature without a result type
// $z - indicates a tuple type will follow
// $_ - indicates an erased type
// $$ - escaped $



export function getEscapedName(name: tube.Identifier): string {
    switch(name.$case) {
        case "Named":
        {
            function isValidStart(ch: string): boolean {
                return ch.match(/_|$|\P{Alphabetic}/u) != null;
            }

            function isValidPart(ch: string): boolean {
                return isValidStart(ch) || ch.match(/\P{General_Category=Decimal_Number}/u) != null;
            }

            let res = "";
            for(const ch of name.name) {
                if(ch == "$") {
                    res += "$$";
                }
                else if(isValidPart(ch)) {
                    if(res.length == 0 && !isValidStart(ch)) {
                        res += "$b";
                    }

                    res += ch;
                }
                else if(ch.length == 1) {
                    res += "$u" + ch.codePointAt(0)!.toString(16).padStart(4, "0");
                }
                else {
                    res += "$U" + ch.codePointAt(0)!.toString(16).padStart(8, "0");
                }
            }

            return res;
        }

        case "Operator":
            return "$o" + getEscapedName({
                $case: "Named",
                name: name.symbol,
            });

        case "Extension":
            return "$e" + getEscapedName(name.inner);

        case "Inverse":
            return "$i" + getEscapedName(name.inner);

        case "Update":
            return "$m" + getEscapedName(name.inner);

        case "FunctionResultValue":
            throw new Error("Function result value should not be used as a declared name.")
    }
}


export function getOverloadExportName(name: tube.Identifier | null, signature: tube.ErasedSignature): string {
    function sigTypePart(t: tube.ErasedSignatureType): string {
        switch(t.$case) {
            case "ClassType":
                return "$c" + importSpecifierPart(t.classImport) + argParts(t.arguments);

            case "TraitType":
                return "$t" + importSpecifierPart(t.traitImport) + argParts(t.arguments);

            case "FunctionType":
                return "$f" + sigTypePart(t.argument) + "$r" + sigTypePart(t.result);

            case "TupleType":
                return "$z" + argParts(t.arguments);

            case "Erased":
                return "$_";
        }
    }

    function importSpecifierPart(specifier: tube.ImportSpecifier): string {
        return specifier.moduleName.tube.name.map(part => "$d" + getEscapedName({ $case: "Named", name: part })).join("") +
            specifier.moduleName.path.path.map(part => "$g" + getEscapedName({ $case: "Named", name: part })).join("") +
            "$n" + getOverloadExportName(specifier.identifier, specifier.signature);
    }

    function argParts(args: readonly tube.ErasedSignatureType[]): string {
        return args.map(arg => "$a" + sigTypePart(arg)).join("");
    }


    let res: string;
    if(name === null) {
        res = "$_";
    }
    else {
        res = getEscapedName(name);
    }

    for(const param of signature.parameterTypes) {
        res += "$p" + sigTypePart(param);
    }

    if(signature.resultType !== undefined) {
        res += "$r" + sigTypePart(signature.resultType);
    }

    return res;
}



