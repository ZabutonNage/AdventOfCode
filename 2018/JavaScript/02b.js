const inputFile = `../_input/02.input`;


console.log(
    solve(getInput(inputFile).split(`\n`))
);


function solve(words) {
    const charSums = getCharSums(words);
    const match = findMatch(charSums);
    return match && intersectStrict(match);
}


function findMatch([current, ...rest]) {
    if (!current) return undefined;

    const [sum, word] = current;
    const candidates = rest.filter(sw => sw[0] <= sum + 25).map(sw => sw[1]);
    const match = getMatch(word, candidates);

    return match || findMatch(rest);
}

function getMatch(a, bs) {
    if (!bs.length) return undefined;

    const splitA = a.split(``);
    const splitB = bs[0].split(``);

    const diff = splitB.map((c, i) => splitA[i].charCodeAt(0) !== c.charCodeAt(0)).filter(x => x).length;
    return diff === 1
        ? [a, bs[0]]
        : getMatch(a, bs.slice(1));
}

function getCharSums(words) {
    return words.map(w => ([charSum(w), w])).sort((a, b) => a[0] - b[0]);

    function charSum(word) {
        return word.split(``).reduce((acc, c) => acc + c.charCodeAt(0), 0);
    }
}

function intersectStrict([a, b]) {
    const as = a.split(``);
    const bs = b.split(``);
    return as.reduce((acc, c, i) => c === bs[i] ? acc + c : acc, ``);
}


function getInput(inputFile) {
    const { join } = require(`path`);
    const { readFileSync } = require(`fs`);

    return readFileSync(join(__dirname, inputFile), `utf8`);
}
