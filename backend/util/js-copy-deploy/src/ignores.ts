import * as fs from 'fs/promises';
import picomatch from 'picomatch';

export type PathPredicate = (filename: string, isDir: boolean) => boolean;

const defaultIgnores = `
.*.swp
._*
.DS_Store
.git
.gitignore
.hg
.npmignore
.npmrc
.lock-wscript
.svn
.wafpickle-*
config.gypi
CVS
npm-debug.log
`;

const forcedIgnores = `
/node_modules/
/package-lock.json
!/package.json
!/README
!/README.*
!/CHANGELOG
!/CHANGELOG.*
!/LICENSE
!/LICENCE
`;

interface Rule {
    negate: boolean;
    dirOnly: boolean;
    match: picomatch.Matcher;
}

function buildRules(patterns: string[]): Rule[] {
    const rules: Rule[] = [];
    for(const raw of patterns) {
        const line = raw.trim();
        if(line.length === 0 || line.startsWith("#")) continue;

        let pat = line;
        const negate = line.startsWith("!");
        if(negate) {
            pat = pat.substring(1);
        }

        if(pat.length === 0) continue;

        if(pat.startsWith("/")) {
            pat = pat.substring(1);
        }
        else {
            pat = "**/" + pat;
        }


        const dirOnly = pat.endsWith("/");
        if(dirOnly) {
            pat = pat.substring(0, pat.length - 1);
        }

        const isMatch = picomatch(pat, {dot: true});
        rules.push({negate, dirOnly, match: isMatch});
    }
    return rules;
}

function composePredicate(rules: Rule[]): PathPredicate {
    return (filename: string, isDir: boolean) => {
        let ignored = false;
        for(const r of rules) {
            if(r.dirOnly && !isDir) {
                continue;
            }

            if(r.negate) {
                if(ignored && r.match(filename)) {
                    ignored = false;
                }
            }
            else {
                if(!ignored && r.match(filename)) {
                    ignored = true;
                }
            }
        }

        return ignored;
    };
}

export async function readNpmIgnore(npmignorePath: string): Promise<PathPredicate> {
    let fileContent;
    try {
        fileContent = await fs.readFile(npmignorePath, "utf8");
    }
    catch {
        fileContent = "";
    }

    const patterns = (defaultIgnores + "\n" + fileContent + "\n" + forcedIgnores)
        .split(/\r?\n/g);

    const rules = buildRules(patterns);
    return composePredicate(rules);
}


