# Print each token in the coffeescript soruce file whose
# name is passed as a parameter.

{ Lexer }    = require './../src/Lexer'
{ print }    = require 'util'
{ keywords } = require './../src/CoffeeScriptParser'
fs           = require 'fs'

fileName = process.argv[2]

fileContent = fs.readFileSync fileName, 'UTF8'

lexer = new Lexer fileContent, keywords

tokens = lexer.tokenize()

for t in tokens
    print "#{lexer.offsetToLine(t.i)}\t#{t}'\n"
