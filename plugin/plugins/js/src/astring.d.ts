
declare module "astring" {
    import type { Node } from "estree";
    
    export function generate(node: Node): string;
}

