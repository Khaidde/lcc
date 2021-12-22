#include "diagnostics.hpp"

namespace lcc {

DxInfo at_point(size_t startI) { return {0, startI, 1}; }

DxInfo curr(Lexer *l) { return {l->line, l->curI, l->curLen}; }

DxInfo at_token(Token *token) { return {token->line, token->startI, token->len}; }

DxInfo at_node(Lexer *l, Node *node) {
    size_t depth = 0;
    size_t ndx = node->endI;
    while (ndx > node->startI) {
        if (l->src->data[ndx] == '/' && ndx - 1 >= node->startI && l->src->data[ndx - 1] == '*') {
            depth++;
        }
        if (depth == 0 && !is_whitespace(l->src->data[ndx])) break;
        if (l->src->data[ndx] == '/' && ndx + 1 < l->src->size && l->src->data[ndx + 1] == '*') {
            depth--;
        }
        ndx--;
    }
    size_t end = ndx;
    ndx = node->startI;
    while (ndx < end) {
        if (l->src->data[ndx + 1] == '\r' || l->src->data[ndx + 1] == '\n') break;
        ndx++;
    }
    return {0, node->startI, ndx - node->startI + 1};
}

DxInfo at_eof(Lexer *l) {
    size_t ndx = l->src->size - 1;
    u32 line = l->line;
    while (ndx >= 0) {
        if (!is_whitespace(l->src->data[ndx])) break;
        if (l->src->data[ndx] == '\n') line--;
        ndx--;
    }
    return {line, ndx + 1, 1};
}

u8 numContextLines = 3;

namespace {

struct LineQueue {
    LStringView *data;
    size_t back{0};
};

LineQueue init_line_queue() {
    LineQueue queue;
    queue.data = mem::malloc<LStringView>(numContextLines);
    return queue;
}

void push_back(LineQueue &queue, const char *src, size_t len) {
    queue.data[queue.back].src = src;
    queue.data[queue.back].len = len;
    queue.back = (queue.back + 1) % numContextLines;
}

LStringView *pop_first(LineQueue &queue) {
    LStringView *rv = &queue.data[queue.back];
    queue.back = (queue.back + 1) % numContextLines;
    return rv;
}

}  // namespace

void display_context(Lexer *l, DxInfo &dxinfo) {
    LineQueue lineQueue = init_line_queue();

    size_t line = 1;
    size_t col = 0;
    size_t ndx = 0;
    while (ndx < l->src->size && ndx < dxinfo.startI) {
        col++;
        if (l->src->data[ndx] == '\n' && line != dxinfo.line) {
            push_back(lineQueue, l->src->data + ndx - col + 1, col - 2);
            col = 0;
            line++;
        }
        ndx++;
    }
    size_t endCol = col;
    size_t endI = ndx;
    while (endI < l->src->size) {
        if (l->src->data[endI] == '\n') {
            endCol--;
            endI--;
            break;
        }
        endCol++;
        endI++;
    }
    push_back(lineQueue, l->src->data + endI - endCol, endCol);

    int ctxLine = line - numContextLines + 1;
    for (int i = 0; i < numContextLines; i++) {
        LStringView *curLine = pop_first(lineQueue);
        int curLineNum = ctxLine + i;
        if (curLineNum >= 1) {
            printf("  %4d %.*s\n", curLineNum, curLine->len, curLine->src);
        }
    }
    printf("%0*s", kHeaderLen + col, "");
    print_color(kAnsiColorRed);
    for (size_t i = 0; i < dxinfo.len; i++) printf("^");
    reset_print_color();
    printf("\n");
}

}  // namespace lcc
