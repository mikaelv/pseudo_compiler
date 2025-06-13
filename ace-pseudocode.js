// Ace Editor mode for pseudo-code syntax highlighting
ace.define("ace/mode/pseudocode_highlight_rules", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text_highlight_rules"], function(require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

    var PseudocodeHighlightRules = function() {
        this.$rules = {
            "start": [
                {
                    token: "keyword.control.pseudocode",
                    regex: "\\b(?:Algorithme|Algorithm|Variables|Debut|Début|Begin|Fin|End|Si|If|Alors|Then|Sinon|Else|Tant que|While|Faire|Do|Pour|For|ET|AND|OU|OR|NON|NOT)\\b"
                },
                {
                    token: "support.function.pseudocode",
                    regex: "\\b(?:Lire|Read|Ecrire|Write)\\b"
                },
                {
                    token: "storage.type.pseudocode",
                    regex: "\\b(?:entier|integer|chaine|string|booleen|boolean|tableau d'entier|arrayint|tableau)\\b"
                },
                {
                    token: "string.quoted.double.pseudocode",
                    regex: '"(?:[^"\\\\]|\\\\.)*"'
                },
                {
                    token: "constant.numeric.pseudocode",
                    regex: "\\b\\d+(?:\\.\\d+)?\\b"
                },
                {
                    token: "keyword.operator.assignment.pseudocode",
                    regex: "<-"
                },
                {
                    token: "keyword.operator.comparison.pseudocode",
                    regex: "[<>=!]="
                },
                {
                    token: "keyword.operator.pseudocode",
                    regex: "[<>=!+\\-*/%]"
                },
                {
                    token: "punctuation.definition.pseudocode",
                    regex: "[{}\\[\\]();,:]"
                },
                {
                    token: "variable.other.pseudocode",
                    regex: "\\b[a-zA-Z_éè][a-zA-Z0-9_éè']*\\b"
                },
                {
                    token: "text",
                    regex: "\\s+"
                }
            ]
        };
    };

    oop.inherits(PseudocodeHighlightRules, TextHighlightRules);

    exports.PseudocodeHighlightRules = PseudocodeHighlightRules;
});

ace.define("ace/mode/pseudocode", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text", "ace/mode/pseudocode_highlight_rules"], function(require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextMode = require("./text").Mode;
    var PseudocodeHighlightRules = require("./pseudocode_highlight_rules").PseudocodeHighlightRules;

    var Mode = function() {
        this.HighlightRules = PseudocodeHighlightRules;
        this.$behaviour = this.$defaultBehaviour;
    };
    oop.inherits(Mode, TextMode);

    (function() {
        this.lineCommentStart = "//";
        this.blockComment = {start: "/*", end: "*/"};
        this.$id = "ace/mode/pseudocode";
    }).call(Mode.prototype);

    exports.Mode = Mode;
});