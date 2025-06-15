const fs = require('fs')
const path = require('path')

let totalTime = BigInt('0')

const performTests = (file) => {
    const tests = JSON.parse(fs.readFileSync(file));

    return tests.forEach(testCase => {
        const { pattern, inputs } = testCase;

        const startTime = process.hrtime.bigint();
        const regex = new RegExp(pattern, 'gm'); // I think it only compiles on first use
        const endTime = process.hrtime.bigint();
        totalTime += endTime - startTime;

        inputs.forEach(input => {
            const { text, matches } = input;

            const startTime = process.hrtime.bigint();
            // Need to turn it into an array, otherwise it's just a (lazy) iterator
            const results = [...text.matchAll(regex)]; 
            const endTime = process.hrtime.bigint();
            totalTime += endTime - startTime;
        });
    });
};

const files = fs.readdirSync('tests/');

console.log("Starting tests...\n[Test Cases]\n");
files.forEach(file => {
    if (path.extname(file) == '.json') performTests('tests/' + file);
});
console.log(`Finished in ${totalTime / BigInt('1000')} μs!\n\n`);
console.log(`[Large Input]\n`)

const patterns = [
    /a/gm,
    /[\w\.+-]+@[\w\.-]+\.[\w\.-]+/gm,
    /[\w]+:\/\/[^/\s?#]+[^\s?#]+(?:\?[^\s#]*)?(?:#[^\s]*)?/gm,
    /(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])/gm
]

const inputText = fs.readFileSync('tests/input-text.txt', 'utf8');

let largeInputTime = BigInt('0');

patterns.forEach(pattern => {
    const startTime = process.hrtime.bigint();
    const results = [...inputText.matchAll(pattern)]; 
    const endTime = process.hrtime.bigint();
    largeInputTime += endTime - startTime;
    totalTime += largeInputTime;
})

console.log(`Finished in ${largeInputTime / BigInt('1000')} μs!\n\n`);

console.log(`[Total]\n`)
console.log(`Finished in ${totalTime / BigInt('1000')} μs!\n\n`);

