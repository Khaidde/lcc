#include "diagnostics.hpp"

#include <cstdarg>

#include "print.hpp"

namespace lcc {

DxInfo curr(Lexer *l) { return {l->finfo, l->line, l->curI, l->curLen}; }

DxInfo at_eof(Lexer *l) {
    size_t ndx = l->finfo->src.size - 1;
    u32 line = l->line;
    while (ndx >= 0) {
        if (!is_whitespace(l->finfo->src.get(ndx))) break;
        if (l->finfo->src.get(ndx) == '\n') line--;
        ndx--;
    }
    return {l->finfo, line, ndx + 1, 1};
}

DxInfo at_point(file::FileInfo *finfo, size_t startI) { return {finfo, (size_t)-1, startI, 1}; }

DxInfo at_token(file::FileInfo *finfo, Token *token) { return {finfo, token->line, token->startI, token->len}; }

DxInfo at_node(file::FileInfo *finfo, Node *node) {
    size_t depth = 0;
    size_t ndx = node->endI;
    while (ndx > node->startI) {
        if (finfo->src.get(ndx) == '/' && ndx - 1 >= node->startI && finfo->src.get(ndx - 1) == '*') {
            depth++;
        }
        if (depth == 0 && !is_whitespace(finfo->src.get(ndx))) break;
        if (finfo->src.get(ndx) == '/' && ndx + 1 < finfo->src.size && finfo->src.get(ndx + 1) == '*') {
            depth--;
        }
        ndx--;
    }
    size_t end = ndx;
    ndx = node->startI;
    while (ndx < end) {
        if (finfo->src.get(ndx + 1) == '\r' || finfo->src.get(ndx + 1) == '\n') break;
        ndx++;
    }
    return {finfo, (size_t)-1, node->startI, ndx - node->startI + 1};
}

namespace {

void dx_out(DxInfo &dxinfo) {
    LStringView *lines = mem::malloc<LStringView>(numContextLines);
    size_t back{0};

    // Print file path if it exists
    if (dxinfo.finfo->path) {
        print_color(kAnsiColorGrey);
        printf("  %s:\n", dxinfo.finfo->path);
        reset_print_color();
    }

    // Get line, col info as well as populate line queue with context info BEFORE the error line
    size_t line = 1;
    size_t col = 0;
    size_t ndx = 0;
    while (ndx < dxinfo.startI) {
        col++;
        if (ndx < dxinfo.finfo->src.size && dxinfo.finfo->src.get(ndx) == '\n') {
            if (line < dxinfo.line) {
                lines[back] = {&dxinfo.finfo->src.get(ndx - col + 1), col - 1};
                back = (back + 1) % numContextLines;
                col = 0;
                line++;
            }
        }
        ndx++;
    }

    // Scan until end of line to grab entire line info AT the error line
    size_t endCol = col;
    size_t endI = ndx;
    while (endI < dxinfo.finfo->src.size) {
        if (endI + 1 < dxinfo.finfo->src.size && dxinfo.finfo->src.get(endI + 1) == '\n') {
            break;
        }
        endCol++;
        endI++;
    }
    lines[back] = {&dxinfo.finfo->src.get(endI - endCol), endCol};
    back = (back + 1) % numContextLines;

    // Print context lines including the error line
    int ctxLine = line - numContextLines + 1;
    for (int i = 0; i < numContextLines; i++) {
        LStringView &curLine = lines[back];
        back = (back + 1) % numContextLines;

        int curLineNum = ctxLine + i;
        if (curLineNum >= 1) printf("  %4d %.*s\n", curLineNum, curLine.len, curLine.src);
    }

    // Add spaces before the error cursor
    printf("%0*s", kHeaderLen + col, "");
};
}  // namespace

u8 numContextLines = 3;

void dx_err(DxInfo dxinfo, const char *format, ...) {
    dx_out(dxinfo);

    // Print error cursor and message
    print_color(kAnsiColorRed);
    for (size_t i = 0; i < dxinfo.len; i++) printf("^");
    printf(" error: ");
    reset_print_color();

    va_list(ap);
    va_start(ap, format);
    vprintf(format, ap);
    va_end(ap);
}

void dx_note(DxInfo dxinfo, const char *format, ...) {
    dx_out(dxinfo);

    // Print error cursor and message
    print_color(kAnsiColorGrey);
    for (size_t i = 0; i < dxinfo.len; i++) printf("^");
    printf(" note: ");
    reset_print_color();

    va_list(ap);
    va_start(ap, format);
    vprintf(format, ap);
    va_end(ap);
}

}  // namespace lcc
