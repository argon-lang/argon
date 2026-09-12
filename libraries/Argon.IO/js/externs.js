import { closeSync, openSync, readSync, writeSync } from "node:fs";

externStaticMethod("path_from_string", (() => {
    const inputStreamType = InputStream$a$r$_.specialize();
    const outputStreamType = OutputStream$a$r$_.specialize();
    const pathType = Path$a$r$_.specialize();

    function createTraitValue(type, methods) {
        const value = Object.create(type.prototype);
        for(const entry of Object.entries(methods)) {
            value[type.methods[entry[0]]] = entry[1];
        }
        return value;
    }

    function createResourcePrototype(resourceType) {
        const type = Resource$a$_$r$_.specialize(resourceType);
        return createTraitValue(type, {
            resource$a$r$_() {
                return this.resource;
            },
            close$a$t$e$r$t$e(_empty) {
                this.close();
                return undefined;
            },
        });
    }

    const inputStreamPrototype = createTraitValue(inputStreamType, {
        read$a$barray$a$bu8$a$e$e$bint$a$e$bint$a$e$_$r$bint$a$e(array, offset, count) {
            return BigInt(readSync(this.file, array, Number(offset), Number(count), null));
        },
    });
    const inputResourcePrototype = createResourcePrototype(inputStreamType);

    const outputStreamPrototype = createTraitValue(outputStreamType, {
        write$a$barray$a$bu8$a$e$e$bint$a$e$bint$a$e$_$r$t$e(array, offset, count) {
            let currentOffset = Number(offset);
            let remaining = Number(count);
            while(remaining > 0) {
                const written = writeSync(this.file, array, currentOffset, remaining);
                currentOffset += written;
                remaining -= written;
            }
            return undefined;
        },
    });
    const outputResourcePrototype = createResourcePrototype(outputStreamType);

    const pathPrototype = createTraitValue(pathType, {
        display$a$r$bstring$a$e() {
            return this.path;
        },
        open_read$a$r$rArgon$dIO$sResource$a$_$r$_$a$rArgon$dIO$sInputStream$a$r$_$a$e$e() {
            const file = openSync(this.path, "r");
            const stream = Object.create(inputStreamPrototype);
            stream.file = file;

            const resource = Object.create(inputResourcePrototype);
            resource.resource = stream;
            resource.close = () => closeSync(file);
            return resource;
        },
        open_write$a$r$rArgon$dIO$sResource$a$_$r$_$a$rArgon$dIO$sOutputStream$a$r$_$a$e$e() {
            const file = openSync(this.path, "w");
            const stream = Object.create(outputStreamPrototype);
            stream.file = file;

            const resource = Object.create(outputResourcePrototype);
            resource.resource = stream;
            resource.close = () => closeSync(file);
            return resource;
        },
    });

    return function path_from_string(path) {
        const value = Object.create(pathPrototype);
        value.path = path;
        return value;
    };
})());
