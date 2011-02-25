this.exports = this unless process?

L = require './log'

OP_CHARS = /[~!@$%^&*()\-=+[\]{}|;:,.<>\/?]/
NUMBER   = /^(0x[0-9a-fA-F]+)|(([0-9]+(\.[0-9]+)?|\.[0-9]+)(e[+\-]?[0-9]+)?)/

#-------------------------------------------------------------------------------
# Token structure
#
# Store a lexing token
# * t: token type. One of:
#      - 'string'
#      - 'keyword' (content can be alphanumeric or punctuations)
#      - 'id' (identifier)
#      - 'number'
#      - 'newline' / 'indent' / 'dedent'
#      - 'javascript'
#      - 'regex'
#      - 'regexFlags'
#      - 'interpStart' / 'interpEnd' / 'interpEsc' / 'interpEnd'
# * v: optional token value:
#      - string content when applicable
#      - indentation level for indent/dedent/newline
# * i: offset at which the token is found in the source string
# * s: is this token separated from the previous one by some spacing?
# * catcode
#-------------------------------------------------------------------------------
class Token
    constructor: (fields) -> (@[k]=v) for k, v of fields
    toString: ->
        "#{@t}[#{@i}#{if @v? then ":'#{@v}'" else ""}#{if @s then ' S' else ''}]"

#-------------------------------------------------------------------------------
#
# TODO: rename adequately.
#
# Registered keywords:
# - alphanumeric keywords are stored as keys, the attached value is true.
# - punctuation-keywords are stored in arrays as values, the corresponding
#   key is their first character.
#   Each array is sorted by decreasing word size.
#
# This set is populated with @addKeyword, it allows to differentiate
# alphanumeric keywords from identifiers, and to recognize multi-characters
# punctuation keywords.
#-------------------------------------------------------------------------------
exports.Keywords = class Keywords
    constructor: (words...) ->
        @set = { }
        @detailedCatcodes = { }
        @add(words...)

    addDetailedCatcode: (catcodes...) ->
        (@detailedCatcodes[cc]=true) for cc in catcodes

    #---------------------------------------------------------------------------
    # Register new keyword(s) for this lexer.
    # TODO: reset any peeked token.
    #---------------------------------------------------------------------------
    add: (words...) ->
        for word in words
            if word.match /^[A-Za-z_]/
                @set[word] = true
            else if (word_len=word.length) > 1
                list = (@set[word[0]] ?= [])
                list_len = list.length
                i = 0
                i++ while i < list_len and list[i].length > word_len
                list.splice(i, 0, word)

    hasWord:            (w) -> @set[w]
    startingWithSymbol: (k) -> @set[k] or []


#-------------------------------------------------------------------------------
# Lexer: turn a source string into a tokens list with method @tokenize().
# The list is intended to be consumed and fed to parsers by Stream.
#
# It would be possible, although overkill given the typical source size
# vs. available RAM, to produce the tokens lazily, by having the Stream
# consumming directly @step() instead of @tokenize().
#
# TODO: the link between Lexer and Stream won't stay strictly one-way:
# a macro, triggered by a parser, can modify the list of keywords, and
# force a re-tokenization of unconsumed tokens. This could make lazy
# tokenization more interesting, as it would reduce complexity from
# o(n^2) to o(n).
#
# TODO: not sure whether char offset -> line conversion should belong to
# Lexer, to Stream, or to a class of its own.
#-------------------------------------------------------------------------------
exports.Lexer = class Lexer

    # field src:                 source code being tokenized
    # field len:                 length of src
    # field keywords:            set of words to be understood as keywords
    # field i:                   current tokenization pointer
    # field indentLevels:        list of used indentation levels
    # field indentChar:          char used to indent, ' ' or '\t'
    # field forbiddenIndentChar: char forbidden for indentation, '\t' or ' '
    # field lineCache:           line number -> offset correspondance table

    #---------------------------------------------------------------------------
    # Get ready to tokenize string `src', with keywords set `keywords'.
    #---------------------------------------------------------------------------
    constructor: (@src, @keywords) ->
        @len = @src.length
        @keywords ?= new Keywords

    #---------------------------------------------------------------------------
    # Build a new token.
    # t: token type; v: optional token value;
    # i: src index, defaults to current index;
    #---------------------------------------------------------------------------
    token: (t, v, i) ->
        i = @i unless i?
        s = @spaced
        catcode = if @keywords.detailedCatcodes[t] then t+"-"+v else t
        @spaced = false
        return new Token {t, v, i, s, catcode}

    #---------------------------------------------------------------------------
    # Return the list of all tokens in @src
    #---------------------------------------------------------------------------
    tokenize: () ->
        @i            = 0
        @indentLevels = [ -1 ]
        @spaced       = true
        tokens        = [@token 'indent', -1]
        @spaced       = true # undone by @token above
        loop
            stepTokens = @step()
            break unless stepTokens?
            tokens = tokens.concat stepTokens
        return tokens[1...-1] # remove dummy indent level -1

    #---------------------------------------------------------------------------
    # Debug trace helper
    #---------------------------------------------------------------------------
    pWhere: (msg) ->
        L.log 'lexer', "#{msg or ''} [#{@i} '#{@src[@i..@i+5].replace( /\n/g,'\\n')}...']"

    #---------------------------------------------------------------------------
    # Perform one step of tokenization..
    # One step of processing can produce more than one token:
    #  * when the next "token" is an interpolated string;
    #  * when several indentations are dedented simultaneously;
    # No token is produced in case of an end-of-line comment
    # (empty list returned).
    #
    # Return:
    # * a list of tokens, most often of length 1;
    # * null when end-of-file is reached.
    #---------------------------------------------------------------------------
    step: ->
        @pWhere 'step'
        j=@i
        @i++ while @src[@i]==' ' or @src[@i]=='\t' #skip whitespace
        @spaced = true unless @i==j
        src_i = @src[@i]
        if not src_i? #EOF, close all pending indents
            return null unless @indentLevels?
            L.log 'algo', "flushing indent levels "+@indentLevels
            x = (@token('dedent', v) for v in @indentLevels.reverse())
            @indentLevels = null
            return x
        else if src_i == '"'
            return @getInterpolation 'string', '"'
        else if src_i == "'"
            return [ @getString 'string' ]
        else if src_i.match /[A-Za-z_]/
            return [ @getWord() ]
        else if src_i == '\n'
            x = @getNewlines()
            @spaced = true
            return x
        else if src_i == '/' and @regexAllowedHere()
            if @src[@i..@i+2] == '///'
                t = @getInterpolation 'regex', '/'
            else t = [ @getString 'regex' ]
            if (t2=@getRegexFlags()) then t.push t2
            return t
        else if src_i == '`'
            return [ @getString 'javascript' ]
        else if src_i.match OP_CHARS # must be after regex and backtick
            return [ @getOp() ]
        else if src_i.match /[0-9]/ or @src[@i..@i+1].match /\.[0-9]/
            return [ @getNumber() ]
        else if src_i == '#' #TODO: handle triple-sharps
            @spaced = true
            @i++ while @src[@i] != '\n'
            return [ ]
        else
            @complain "Unexpected char `#{src_i}'."



    #---------------------------------------------------------------------------
    # #-------------------------------------------------------------------------
    # #
    # # Specialized token extractors (called by @step()).
    # #
    #-#-------------------------------------------------------------------------
    #---------------------------------------------------------------------------

    #---------------------------------------------------------------------------
    # Generate indent/dedent/newline tokens.
    # Return a list of one or several tokens.
    #
    # The method is cut in two stages:
    # 1- Determine indentation on the new line:
    #    loop until a line with some non-blank, non-comment characters is found
    #    (i.e. comments-only lines are skipped)
    #    i = beginning of line, j = first non-indentation char
    # 2- Compare this current indentation with previous ones, kept in list
    #    @indenLevels
    #
    # Until the first indentation character is found, both '\t' and ' ' are
    # accepted. After one of these characters has been found, the other
    # becomes illegal for indentation in the whole file.
    #---------------------------------------------------------------------------
    getNewlines: ->
        # 1- determine indentation
        offset = i = @i
        loop
            i++ while @src[i] == '\n'
            if not @indentChar?
                src_i = @src[i]
                # Determine the indentation char (' ' or '\t')
                if src_i == '\t'
                    @indentChar = '\t'
                    @forbiddenIndentChar = ' '
                else if src_i == ' '
                    @indentChar = ' '
                    @forbiddenIndentChar = '\t'
            j = i
            if @indentChar
                j++ while @src[j] == @indentChar
            src_j = @src[j]
            if src_j == '\n' # blank line
                i=j+1
            else if src_j == '#' # comment-only line
                i=j+1
                i++ until @src[i] == '\n'
            else if not src_j
                L.log 'lexer', 'finalspace'
                @i = j
                return [ ] # only spaces up to EOF, no token
            else if src_j == @forbiddenIndentChar
                @complain "don't mix tabs and spaces in indentation"
            else break

        # 2- generate tokens
        @i = j
        indentLevel = j-i
        lastIndentLevelIdx = @indentLevels.length-1
        previousLevel = @indentLevels[lastIndentLevelIdx]
        if previousLevel == indentLevel
            return [ @token 'newline', indentLevel, offset ]
        else if previousLevel < indentLevel
            @indentLevels.push indentLevel
            return [ @token 'indent', indentLevel, offset ]
        else
            results = [ ]
            while @indentLevels[lastIndentLevelIdx] > indentLevel
                lastIndentLevelIdx--
                poppedLevel = @indentLevels.pop()
                results.push @token 'dedent', poppedLevel, offset
            results.push @token 'newline', indentLevel, offset
            if @indentLevels[lastIndentLevelIdx] != indentLevel
                @complain "dedenting to an unknown indentation level"
            return results

    #---------------------------------------------------------------------------
    # Parse an interpolated string or regex.
    # Return a list of tokens.
    # Arguments
    #  * type:            type of token to be produced, 'string' or 'regex'
    #  * delimiterChar:   first delimiting character, '"' or '/'
    #  * tripleDelimiter: '"""', '///' or false
    #
    # TODO: handle indentation fixing for triple-double-quotes.
    #---------------------------------------------------------------------------
    getInterpolation: (type, delimiterChar) ->
        tripleDelimiter = if @src[@i]==@src[@i+1]==@src[@i+2] then @src[@i..@i+2] else false
        results         = [ ]
        i               = if tripleDelimiter then @i+3 else @i+1
        j               = i-1
        interpStarted   = false
        loop
            j++
            @complain "unterminated string" if @len==j
            k=@src[j]
            continue unless k == delimiterChar or k == '#'
            continue if @src[j-1] == '\\'
            if k == delimiterChar
                if tripleDelimiter
                    if @src[j..j+2] == tripleDelimiter then @i=j+3 else continue
                else @i=j+1
                # End of string
                unless i==j and results.length>0
                    results.push @token type, @src[i...j], i
                break
            else if @src[j+1] == '{' # we already know that @src[j]=='#'
                # Interpolation
                braceLevel = 1
                unless interpStarted
                    results.push @token 'interpStart', tripleDelimiter or delimiterChar
                    interpStarted=true
                results.push @token type, @src[i...j], i unless i==j
                @i = j+2 # skip '#{'
                results.push @token 'interpEsc', null, j
                loop # iterate until the escape "#{ ... }" is closed
                    x = @step()
                    if not x?
                        @complain "Unterminated interpolation"
                    else if x.length==1 and (x_0=x[0]).t=='keyword'
                        if (x_0_v=x_0.v) == '}'
                            braceLevel--
                            break if braceLevel==0
                        else if x_0_v == '{'
                            braceLevel++
                    results = results.concat (x)
                i = @i; j = i-1
                results.push @token 'interpUnesc', null, j
        results.push @token 'interpEnd', tripleDelimiter or delimiterChar, @i-1 if interpStarted
        return results

    #---------------------------------------------------------------------------
    # Parse a non-interpolated string, either with simple or triple delimiter.
    # The delimiter character is determined by looking directly at @src[@i].
    # The token type is passed as parameter.
    # Return a single string token.
    #
    # TODO: check whether/how triple-delims can be escaped within the string.
    #---------------------------------------------------------------------------
    getString: (type) ->
        i         = @i
        delimiter = @src[i]
        content   = null
        if @src[i]==@src[i+1]==@src[i+2] # triple-delimiter string
            i += 3; j = i
            loop
                j++ while j<@len and @src[j] != delimiter
                if @src[j..j+2] == triple
                    # TODO: check if triple quotes can be escaped with a '\'
                    @i = j+3
                    content = @reindentString @unescape @src[i...j]
                    break
                else if j == @len
                    @complain "unterminated #{type}"
                else
                    j += 2
        else # simple-delimiter string
            i++; j=i
            j++ until j>@len or @src[j] == delimiter and @src[j-1] != '\\'
            @complain "unterminated #{type}" if j == @len
            @i = j+1
            content = @unescape @src[i...j]
        return @token type, content, i

    #---------------------------------------------------------------------------
    # Try to fetch regex flags after a regex, return it or null.
    # TODO: Check whether duplicate flags are allowed, filter out if not.
    #---------------------------------------------------------------------------
    getRegexFlags: ->
        return if @spaced
        MAX = 6
        flags = @src[@i...@i+MAX].match(/^[imgy]*/)[0]
        len = flags.length
        @complain "too many regex flags" if len==MAX
        return null if len==0
        offset=@i
        @i += len
        return @token 'regexFlags', flags, offset

    #---------------------------------------------------------------------------
    # Extract a word.
    # Return a single token.
    #
    # TODO: sort words between keywords and identifiers.
    #---------------------------------------------------------------------------
    getWord: ->
        i=@i
        j=i+1
        j++ while j<@len and @src[j].match /[A-Za-z0-9_]/
        @i=j
        content = @src[i...j]
        kind = if @keywords.hasWord(content) then 'keyword' else 'id'
        return @token kind, content, i

    #---------------------------------------------------------------------------
    # Extract an operator.
    # Return a single token.
    #
    # TODO: keep and use a set of multi-character operators.
    #---------------------------------------------------------------------------
    getOp: ->
        for candidate in @keywords.startingWithSymbol(@src[@i])
            len = candidate.length
            if @src[@i...@i+len] == candidate
                start = @i
                @i += len
                return @token 'keyword', candidate, start

        # Default case = single-char keyword
        return @token 'keyword', @src[@i], @i++

    #---------------------------------------------------------------------------
    # Extract a number.
    # TODO: consider using native parseFloat()
    #---------------------------------------------------------------------------
    getNumber: ->
        MAX_SIZE = 32
        x = @src[@i..@i+MAX_SIZE].match NUMBER
        num = x?[0]
        @complain "number too long" if num.length > MAX_SIZE
        t = @token 'number', num
        @i += num.length
        return t

    #---------------------------------------------------------------------------
    # Extract a block of verbatim Javascript code.
    # Return a single token.
    #---------------------------------------------------------------------------
    getJavaScript: ->
        i = j = @i+1
        ++j until (unterminated = j>=@len) or @src[j]=='`' and @src[j-1]!='\\'
        @complain "Unterminated javascript sequence" if unterminated
        @i=j+1
        return @token 'javascript', @src[i...j]



    #---------------------------------------------------------------------------
    # #-------------------------------------------------------------------------
    # #
    # # Misc. helpers
    # #
    #-#-------------------------------------------------------------------------
    #---------------------------------------------------------------------------

    #---------------------------------------------------------------------------
    # Revert escape sequences in a string content into the chars they stand for.
    #
    # TODO: unescaped strings will have to be mostly reescaped at dump.
    # It might be wise to store them under their javascript-ready form.
    # Anyway, line returns must be JSified.
    #---------------------------------------------------------------------------
    unescape: (str) -> str

    #---------------------------------------------------------------------------
    # Produce and throw a lexing error exception
    #---------------------------------------------------------------------------
    complain: (msg) ->
        throw new Error "SyntaxError: "+msg

    #---------------------------------------------------------------------------
    # Fix indentation for triple-delimiter strings.
    # Syntax errors might be thrown upon inconsistent indentation.
    # NB: the indentation character must already be knwon at this stage,
    # or the indentation level must be zero.
    #---------------------------------------------------------------------------
    reindentString: (str) -> str

    #---------------------------------------------------------------------------
    # Is it legal to have a regex starting at @i?
    # TODO: check presence of a number as previous token.
    #---------------------------------------------------------------------------
    regexAllowedHere: ->
        return false unless @spaced
        i=@i+1
        loop
            src_i=@src[i++]
            return false if src_i=='\n'
            return true  if src_i=='/'

    #---------------------------------------------------------------------------
    # Retrieve a line number from an offset in @src.
    #
    # `lineCache' associates a line number to the offset of that line's
    # first character. '\n' chars are considered part of the previous line.
    #
    # The rationales behind calculating line numbers on demand are:
    #  * it's easier to keep track of offset than lines in the lexer code;
    #  * exact offsets might be useful for refactoring libs;
    #  * when no error msg is needed, no line number needs to be computed;
    #  * when a compilation error msg is needed, speed is not an issue anymore;
    #  * all of the line counting code is here, not scattered everywhere.
    #
    #---------------------------------------------------------------------------
    offsetToLine: (offset) ->
        lastLine   = @lineCache.length-1
        lastOffset = @lineCache[lastLine]

        if offset < lastOffset # Retrieve from cache.
            # It could be done faster by dichotomy in theory; but in practice,
            # most recurring accesses will probably be made in sequence.
            for line_i in [lastLine..0]
                offset_i = @lineCache[line_i]
                return line_i+1 if offset > offset_i

        else if i>=@len
            return -1

        else # Extend cache
            i = lastOffset # invariant: cache is filled up to line #i
            while offset >= i
                i++ until i >= @len or @src[i] == '\n' # i offset of next '\n'
                @lineCache[++lastLine] = ++i
            return lastLine

    # line number, starting at 0 -> offset cache, used by @offsetToLine
    lineCache: [-1]



#-------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------
# #
# # Stream reading API.
# #
#-#-----------------------------------------------------------------------------
#-------------------------------------------------------------------------------
exports.Stream = class Stream

    #---------------------------------------------------------------------------
    # Stream public interface:
    # @peek(n): return the n-th token without consuming it. n?=1.
    # @next(n): consume and return the n-th token. n?=1.
    # @indentation(n): return indentation level of the n-th token. n?=1.
    # @save(): return a state, allowing to restore the current stream's state.
    # @restore(state): revert any changes made since @save returned state.
    #---------------------------------------------------------------------------

    # field index: index of last consummed token in @tokens
    # field tokens: all tokens produced by the lexer
    # field currentIndentation: indentation of the last consummed token

    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    constructor: (lexer) ->
        @tokens = lexer.tokenize()
        @index  = -1
        @currentIndentation = 0

    eof: new Token 'eof'

    #---------------------------------------------------------------------------
    # Return the n-th next token, without consumming it.
    #
    # TODO: generate tokens lazily, CTMP will be able to change the set of
    # active keywords while parsing/tokenizing.
    #---------------------------------------------------------------------------
    peek: (n) ->
        n ?= 1
        # L.log 'lexer', "? peek #{@tokens[@readIndex+n]}\n"
        return @tokens[@index+n] or @eof

    #---------------------------------------------------------------------------
    # Return the n-th next token, remove all tokens up to it from token stream.
    #---------------------------------------------------------------------------
    next: (n) ->
        n ?= 1
        result = @peek(n)
        for tok in @tokens[@index+1 .. @index+n]
                L.log 'lexer', ">>> consumed #{tok.t} #{tok.v ? ''}"
                if (tok_t=tok.t) == 'indent' or tok_t == 'dedent'
                    @currentIndentation = tok.v
        @index += n
        return result or @eof

    #---------------------------------------------------------------------------
    # Return the indentation level of the n-th next token.
    #---------------------------------------------------------------------------
    indentation: (n) ->
        n ?= 1
        result = @currentIndentation
        for tok in @tokens[@index+1 .. @index+n]
            if (tok_t=tok.t) == 'indent' or tok_t == 'dedent'
                result = tok.v
        return result

    #---------------------------------------------------------------------------
    # Save and restore reading positions.
    # Allow a parser to undo some readings in case of late failure.
    #---------------------------------------------------------------------------
    save:            -> [@index, @currentIndentation]
    restore: (state) -> [@index, @currentIndentation] = state
