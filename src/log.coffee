u = require 'util'
this.exports = this unless process?

modules = { }
indentLevels = { }
longestModLength = false # to be set later
exports.add = (mod, tab) -> modules[mod] = tab or true
exports.del = (mod)      -> modules[mod] = undefined

require './logconfig.coffee'

log = exports.log = (mod, msg) ->

    tab = modules[mod]
    return unless tab # not logged

    # indent if it is an tab-indented log
    if typeof tab is 'string'
        ind = (tab for _ in [0...indentLevels[mod]]).join ''
    else ind = ''

    # compute the padding to align tags if needed
    unless longestModLength
        longestModLength = 0
        longestModLength = l for m of modules when (l=m.length)>longestModLength
        u.print "longestModLength = #{longestModLength}\n"

    padding = "                 "[...longestModLength-mod.length]

    # output
    u.print("[#{mod}]#{padding} #{ind}#{msg}\n")

exports.logindent = (mod, msg) ->
    indentLevels[mod] ?= 0
    indentLevels[mod]++
    log(mod, msg) if msg

exports.logdedent = (mod, msg) ->
    log(mod, msg) if msg
    indentLevels[mod]--
