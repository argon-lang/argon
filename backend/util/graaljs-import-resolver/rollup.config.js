import { nodeResolve } from "@rollup/plugin-node-resolve";

export default {
    input: "lib/index.js",
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
    ],
};

