const inputFile = `../_input/03.input`;

const Nothing = {};


console.log(
    solve(getInput(inputFile).split(`\n`).map(parseClaim))
);


function solve(claims) {
    return getNonOverlapping(claims, 0).fromMaybe(`No non-overlapping claim found`);
}


function getNonOverlapping(claims, i) {
    if (!claims[i]) return Maybe();

    const solitary = getNonOverlappingInner(claims, i, 0);
    return solitary.isNothing()
        ? getNonOverlapping(claims, i +1)
        : solitary.map(s => s.id);
}

function getNonOverlappingInner(claims, i, j) {
    if (i === j) return getNonOverlappingInner(claims, i, j +1);

    const claimA = claims[i];
    const claimB = claims[j];
    if (!claimB) return Maybe(claimA);

    const overlap = claimOverlap(claimA, claimB);
    return overlap.isNothing()
        ? getNonOverlappingInner(claims, i, j +1)
        : Maybe();
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
            : value,
        isNothing: () => isNothing(value)
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
