#!/usr/bin/env node

import {copyPackage} from "./index.js";


const args = process.argv.slice(2);

if(args.length !== 2) {
    console.error('Usage: copy-package <source> <destination>');
    process.exit(1);
}

const [source, destination] = args;

try {
    await copyPackage(source!, destination!);
    console.log(`Successfully copied package from ${source} to ${destination}`);
}
catch(error) {
    console.error('Failed to copy package:', error instanceof Error ? error.message : String(error));
    process.exitCode = 1;
}

