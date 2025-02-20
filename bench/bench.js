const fs = require('fs')

let totalTime = BigInt('0')

const performTests = (file) => {
    const tests = JSON.parse(fs.readFileSync(file));

    return tests.forEach(testCase => {
        const { pattern, inputs } = testCase;

        const startTime = process.hrtime.bigint();
        const regex = new RegExp(pattern, 'g'); // I think it only compiles on first use
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

console.log("Starting tests.\n");
files.forEach(file => performTests('tests/' + file));
console.log(`Finished in ${totalTime / BigInt('1000')} μs!\n`);

