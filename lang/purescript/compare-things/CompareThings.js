exports.parseIntImpl = function(str) {
  var parsed = +(str)
  return isNaN(parsed) ? null : parsed
}
