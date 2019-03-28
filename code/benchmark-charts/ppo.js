const fs = require('fs')
const { ppChart, writeResult } = require('./charts')

if (process.argv.length < 4) {
  console.log('Usage: ppo.js [input-data] [output-chart]')
  process.exit(1)
}

const input = process.argv[2]
const output = process.argv[3]

const data = fs.readFileSync(input).toString()
const result = ppChart(JSON.parse(data), "Objective ratio (lower the better)")

writeResult(output, result, (err) => {
  if (!err) {
    console.log('Generated performance profile chart to ' + output)
  }
})