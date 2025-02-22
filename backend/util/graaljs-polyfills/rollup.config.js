import { nodeResolve } from "@rollup/plugin-node-resolve";
import commonjs from '@rollup/plugin-commonjs';

export default {
    input: "src/index.js",
    output: {
        file: "dist/dist.js",
        format: "es"
    },
    plugins: [
        nodeResolve({
            exportConditions: [
                "graal"
            ],
        }),
        commonjs(),
    ],
};

