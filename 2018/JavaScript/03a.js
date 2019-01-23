const inputFile = `../_input/03.input`;

const Nothing = {};


console.log(
    solve(getInput(inputFile).split(`\n`).map(parseClaim))
);


function solve(claims) {
    const overlaps = claims.reduce((acc, claim, i, claims) => {
        const overlap = claims.slice(i +1).reduce(
            (overlaps, c) => claimOverlap(claim, c).map(co => overlaps.add(co)).fromMaybe(overlaps)
            , new Set()
        );
        return overlap.size ? acc.add(overlap) : acc;
    }, new Set());

    const overlapsMap = Array.from(overlaps).reduce(
        (map, perClaim) => Array.from(perClaim).reduce(mergeOverlap, map)
        , new Map()
    );

    return Array.from(overlapsMap).reduce((acc, kv) => acc + kv[1].size, 0);
}


function mergeOverlap(map, overlap) {
    const rngHoriz = range(overlap.left, overlap.right);
    const rngVert = range(overlap.top, overlap.bottom);

    return rngHoriz.reduce((acc, left) => acc.set(left, new Set([...(acc.get(left) || []), ...rngVert])), map);
}

function claimOverlap(a, b) {
    if (b.left > a.right || a.left > b.right) return Maybe();
    if (b.top > a.bottom || a.top > b.bottom) return Maybe();

    return Maybe({
        left: Math.max(a.left, b.left),
        right: Math.min(a.right, b.right),
        top: Math.max(a.top, b.top),
        bottom: Math.min(a.bottom, b.bottom)
    });
}

function parseClaim(claim) {
    const [_, id, x, y, width, height] = /#(\d+) @ (\d+),(\d+): (\d+)x(\d+)/.exec(claim);
    const xInt = parseInt(x);
    const yInt = parseInt(y);
    const wInt = parseInt(width);
    const hInt = parseInt(height);
    return {
        id: parseInt(id),
        left: xInt,
        right: xInt + (wInt -1),
        top: yInt,
        bottom: yInt + (hInt -1),
        width: wInt,
        height: hInt
    };
}

function range(from, to) {
    return Array.from(Array(to - from +1), (_, i) => i + from);
}


function Maybe(value) {
    return Object.freeze({
        map: f => isNothing(value)
            ? Maybe(Nothing)
            : Maybe(f(value)),
        fromMaybe: defVal => isNothing(value)
            ? defVal
            : value,
        fromMaybe$: defFn => isNothing(value)
            ? defFn()
            : value
    });

    function isNothing(val) {
        return val === undefined
            || val === Nothing
            || val === null
            || Number.isNaN(val);
    }
}


function getInput(inputFile) {
    const { join } = require(`path`);
    const { readFileSync } = require(`fs`);

    return readFileSync(join(__dirname, inputFile), `utf8`);
}
