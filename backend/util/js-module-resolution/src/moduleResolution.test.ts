import { describe, it, expect } from "vitest";
import {
  ModuleResolution,
  type ESMResolution,
  NonFileUrlError,
  InvalidModuleSpecifierError,
  UnsupportedDirectoryImport,
  ModuleNotFoundError,
  PackagePathNotExportedError,
  PackageImportNotDefinedError,
  InvalidPackageTargetError,
  InvalidPackageConfigurationError,
} from "./index.js";

function u8(str: string): Uint8Array {
  return new TextEncoder().encode(str);
}

function makeFs(files: Record<string, string>): ReadonlyMap<string, Uint8Array> {
  const map = new Map<string, Uint8Array>();
  for (const [path, content] of Object.entries(files)) {
    map.set(path.startsWith("/") ? path : "/" + path, u8(content));
  }
  return map;
}

function fileUrl(path: string): URL {
  if (!path.startsWith("/")) path = "/" + path;
  return new URL("file://" + path);
}

describe("ModuleResolution.esmResolve basic file resolution", () => {
  it("resolves a relative file next to parent", () => {
    const fs = makeFs({
      "/proj/app/main.mjs": "export {}\n",
      "/proj/app/dep.js": "export {}\n",
    });
    const mr = new ModuleResolution(fs);
    const parent = fileUrl("/proj/app/main.mjs");
    const res: ESMResolution = mr.esmResolve("./dep.js", parent);

    expect(res.resolved.pathname).toBe("/proj/app/dep.js");
    // No package scope for plain /proj/app, so format is undefined for .js
    expect(res.format).toBeUndefined();
  });

  it("throws for directory import", () => {
    const fs = makeFs({
      "/proj/app/main.mjs": "export {}\n",
      "/proj/app/dir/file.js": "export {}\n",
    });
    const mr = new ModuleResolution(fs);
    const parent = fileUrl("/proj/app/main.mjs");
    expect(() => mr.esmResolve("./dir", parent)).toThrow(UnsupportedDirectoryImport);
  });

  it("throws for missing file", () => {
    const fs = makeFs({ "/a/main.mjs": "export {}\n" });
    const mr = new ModuleResolution(fs);
    const parent = fileUrl("/a/main.mjs");
    expect(() => mr.esmResolve("./missing.mjs", parent)).toThrow(ModuleNotFoundError);
  });

  it("throws for non-file protocol URLs", () => {
    const fs = makeFs({ "/a/main.mjs": "export {}\n" });
    const mr = new ModuleResolution(fs);
    const parent = fileUrl("/a/main.mjs");
    expect(() => mr.esmResolve("data:text/javascript,export%20{}", parent)).toThrow(NonFileUrlError);
  });
});

describe("ModuleResolution package resolution (node_modules)", () => {
  it("resolves package using exports main string", () => {
    const fs = makeFs({
      "/proj/app/main.mjs": "export {}\n",
      "/proj/app/node_modules/pkg/package.json": JSON.stringify({
        name: "pkg",
        exports: "./index.mjs",
      }),
      "/proj/app/node_modules/pkg/index.mjs": "export {}\n",
    });
    const mr = new ModuleResolution(fs);
    const parent = fileUrl("/proj/app/main.mjs");
    const res = mr.esmResolve("pkg", parent);
    expect(res.resolved.pathname).toBe("/proj/app/node_modules/pkg/index.mjs");
    expect(res.format).toBe("module");
  });

  it("resolves package using main when no exports", () => {
    const fs = makeFs({
      "/proj/app/main.mjs": "export {}\n",
      "/proj/app/node_modules/pkg/package.json": JSON.stringify({
        name: "pkg",
        main: "main.mjs",
      }),
      "/proj/app/node_modules/pkg/main.mjs": "export {}\n",
    });
    const mr = new ModuleResolution(fs);
    const parent = fileUrl("/proj/app/main.mjs");
    const res = mr.esmResolve("pkg", parent);
    expect(res.resolved.pathname).toBe("/proj/app/node_modules/pkg/main.mjs");
    expect(res.format).toBe("module");
  });

  it("resolves subpath exports map", () => {
    const fs = makeFs({
      "/proj/app/main.mjs": "export {}\n",
      "/proj/app/node_modules/pkg/package.json": JSON.stringify({
        name: "pkg",
        exports: {
          ".": "./index.mjs",
          "./feature": "./feat.mjs",
        },
      }),
      "/proj/app/node_modules/pkg/index.mjs": "export {}\n",
      "/proj/app/node_modules/pkg/feat.mjs": "export {}\n",
    });
    const mr = new ModuleResolution(fs);
    const parent = fileUrl("/proj/app/main.mjs");
    const res = mr.esmResolve("pkg/feature", parent);
    expect(res.resolved.pathname).toBe("/proj/app/node_modules/pkg/feat.mjs");
  });

  it("throws when subpath not exported", () => {
    const fs = makeFs({
      "/proj/app/main.mjs": "export {}\n",
      "/proj/app/node_modules/pkg/package.json": JSON.stringify({
        name: "pkg",
        exports: {
          ".": "./index.mjs",
        },
      }),
      "/proj/app/node_modules/pkg/index.mjs": "export {}\n",
    });
    const mr = new ModuleResolution(fs);
    const parent = fileUrl("/proj/app/main.mjs");
    expect(() => mr.esmResolve("pkg/other", parent)).toThrow(PackagePathNotExportedError);
  });
});

describe("ModuleResolution package imports (# aliases)", () => {
  it("resolves #imports defined in nearest package.json", () => {
    const fs = makeFs({
      "/proj/app/package.json": JSON.stringify({
        name: "app",
        imports: {
          "#foo": "./aliased.mjs",
        },
      }),
      "/proj/app/aliased.mjs": "export {}\n",
      "/proj/app/src/main.mjs": "export {}\n",
    });
    const mr = new ModuleResolution(fs);
    const parent = fileUrl("/proj/app/src/main.mjs");
    const res = mr.esmResolve("#foo", parent);
    expect(res.resolved.pathname).toBe("/proj/app/aliased.mjs");
    expect(res.format).toBe("module");
  });

  it("throws when # import is not defined", () => {
    const fs = makeFs({
      "/proj/app/package.json": JSON.stringify({ name: "app" }),
      "/proj/app/src/main.mjs": "export {}\n",
    });
    const mr = new ModuleResolution(fs);
    const parent = fileUrl("/proj/app/src/main.mjs");
    expect(() => mr.esmResolve("#missing", parent)).toThrow(PackageImportNotDefinedError);
  });

  it("throws for invalid # import target outside package", () => {
    const fs = makeFs({
      "/proj/app/package.json": JSON.stringify({
        name: "app",
        imports: {
          "#bad": "../evil.mjs",
        },
      }),
      "/proj/app/src/main.mjs": "export {}\n",
    });
    const mr = new ModuleResolution(fs);
    const parent = fileUrl("/proj/app/src/main.mjs");
    expect(() => mr.esmResolve("#bad", parent)).toThrow(InvalidPackageTargetError);
  });

  it("rejects invalid # specifiers beginning with #/", () => {
    const fs = makeFs({
      "/proj/app/package.json": JSON.stringify({ name: "app", imports: {} }),
      "/proj/app/src/main.mjs": "export {}\n",
    });
    const mr = new ModuleResolution(fs);
    const parent = fileUrl("/proj/app/src/main.mjs");
    expect(() => mr.esmResolve("#/%66oo", parent)).toThrow(InvalidModuleSpecifierError);
  });
});

describe("ModuleResolution edge cases and errors", () => {
  it("throws InvalidPackageConfigurationError for numeric condition keys in object target", () => {
    const fs = makeFs({
      "/proj/app/main.mjs": "export {}\n",
      "/proj/app/node_modules/pkg/package.json": JSON.stringify({
        name: "pkg",
        exports: {
          default: { "123": "./x.mjs" },
        },
      }),
      "/proj/app/node_modules/pkg/x.mjs": "export {}\n",
    });
    const mr = new ModuleResolution(fs);
    const parent = fileUrl("/proj/app/main.mjs");
    expect(() => mr.esmResolve("pkg", parent)).toThrow(InvalidPackageConfigurationError);
  });
});
