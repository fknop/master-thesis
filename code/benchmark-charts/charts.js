const fs = require('fs');
const mkdirp = require('./mkdirp');
const getDirName = require('path').dirname;

function readTemplate (template) {
  return fs.readFileSync(template).toString()
}

function replace (template, data) {
  return template.replace('{ /** DATA **/ }', JSON.stringify(data, null, 4))
}

function chart (template, data) {
  return replace(readTemplate(template), data)
}

module.exports.lineChart = function (data) {
  return chart('templates/line-chart.html', data)
}

module.exports.columnChart = function (data) {
  return chart('templates/column-chart.html', data)
}

module.exports.writeResult = function (path, result, cb) {
  mkdirp(getDirName(path), function (err) {
    if (err) return cb(err);
    fs.writeFile(path, result, cb);
  });
}