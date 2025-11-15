#include "hw7.h"

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if(root == NULL){
        bst_sf *node = malloc(sizeof(bst_sf));
        node->mat = mat;
        node->left_child = NULL;
        node->right_child = NULL;
        return node;
    }

    if(mat->name < root->mat->name){
        root->left_child = insert_bst_sf(mat, root->left_child);
    }
    else{
        root->right_child = insert_bst_sf(mat, root->right_child);
    }

    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if(root == NULL){
        return NULL;
    }

    if(name == root->mat->name){
        return root->mat;
    }
    else if(name < root->mat->name){
        return find_bst_sf(name, root->left_child);
    }
    else{
        return find_bst_sf(name, root->right_child);
    }
}

void free_bst_sf(bst_sf *root) {
    if(root == NULL) return;

    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);

    free(root->mat);
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    matrix_sf *result = malloc(sizeof(matrix_sf) + mat1->num_cols * mat1->num_rows * sizeof(int));
    unsigned int nums = mat1->num_cols * mat1->num_rows;

    result->name = 'A';
    result->num_rows = mat1->num_rows;
    result->num_cols = mat1->num_cols;

    for(unsigned int i = 0; i < nums; i++){
        result->values[i] = mat1->values[i] + mat2->values[i];
    }
    return result;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    matrix_sf *result = malloc(sizeof(matrix_sf) + mat1->num_rows * mat2->num_cols * sizeof(int));
    
    result->name = 'M';
    result->num_rows = mat1->num_rows;
    result->num_cols = mat2->num_cols;

    for(unsigned int i = 0; i < mat1->num_rows; i++){
        for(unsigned int j = 0; j < mat2->num_cols; j++){
            int sum = 0;
            for(unsigned int k = 0; k < mat1->num_cols; k++){
                sum += mat1->values[i * mat1->num_cols + k] * mat2->values[k * mat2->num_cols + j];
            }
            result->values[i * mat2->num_cols + j] = sum;
        }
    }
    return result;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    matrix_sf *result = malloc(sizeof(matrix_sf) + mat->num_cols * mat->num_rows * sizeof(int));
    unsigned int nums = mat->num_cols * mat->num_rows;

    result->name = 'T';
    result->num_rows = mat->num_cols;
    result->num_cols = mat->num_rows;

    for(unsigned int i = 0; i < nums; i++){
        result->values[i % mat->num_cols * mat->num_rows + i / mat->num_cols] = mat->values[i];
    }
    return result;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    unsigned int num_rows = 0, num_cols = 0;
    const char *ptr = expr;

    while(isspace(*ptr)){
        ptr++;
    }
    num_rows = strtol(ptr, &ptr, 10);

    while(isspace(*ptr)){
        ptr++;
    }
    num_cols = strtol(ptr, &ptr, 10);

    while(*ptr != '['){
        ptr++;
    }
    ptr++;

    matrix_sf *result = malloc(sizeof(matrix_sf) + num_cols * num_rows * sizeof(int));
    result->name = name;
    result->num_cols = num_cols;
    result->num_rows = num_rows;

    for(unsigned int i = 0; i < num_cols * num_rows; i++){
        while(isspace(*ptr) || *ptr == ';'){
            ptr++;
        }
        result->values[i] = strtol(ptr, &ptr, 10);
    }

    return result;
}

char* infix2postfix_sf(char *infix) {
    int len = strlen(infix), index = 0, top = -1;
    char* postfix = malloc(len + 1);
    char* stack = malloc(len + 1);

    for(int i = 0; i < len; i++){
        char c = infix[i];

        if(isalpha(c)){
            postfix[index++] = c;
        }
        else if(isspace(c)){
            continue;
        }
        else if(c == '('){
            stack[++top] = c;
        }
        else if(c == ')'){
            while(top >= 0 && stack[top] != '(') {
                postfix[index++] = stack[top--];
            }
            top--;
        }
        else if(c == '\''){
            postfix[index++] = c;
        }
        else if(c == '+'){
            while(top >= 0 && (stack[top] == '+' || stack[top] == '*')){
                postfix[index++] = stack[top--];
            }
            stack[++top] = c;
        }
        else if(c == '*'){
            while(top >= 0 && stack[top] == '*'){
                postfix[index++] = stack[top--];
            }
            stack[++top] = c;
        }
    }

    while (top >= 0) {
        postfix[index++] = stack[top--];
    }
    
    postfix[index] = '\0';
    free(stack);
    return postfix;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    char *postfix = infix2postfix_sf(expr);
    int len = strlen(postfix);
    matrix_sf **stack = malloc(len * sizeof(matrix_sf*));
    int top = -1;
    char temp_name = 1;

    for(int i = 0; i < len; i++){
        char c = postfix[i];
        
        if(isspace(c)){
            continue;
        }
        else if(isalpha(c)){
            matrix_sf *matrix = find_bst_sf(c, root);
            stack[++top] = matrix;
        }
        else if(c == '\''){
            matrix_sf *matrix = stack[top--];
            matrix_sf *result = transpose_mat_sf(matrix);
            result->name = temp_name++;

            if(!isalpha(matrix->name)){
                free(matrix);
            }
            stack[++top] = result;
        }
        else if(c == '+'){
            matrix_sf *matrix1 = stack[top--];
            matrix_sf *matrix2 = stack[top--];
            matrix_sf *result = add_mats_sf(matrix1, matrix2);
            result->name = temp_name++;

            if(!isalpha(matrix1->name)){
                free(matrix1);
            }
            if(!isalpha(matrix2->name)){
                free(matrix2);
            }

            stack[++top] = result;
        }
        else if(c == '*'){
            matrix_sf *matrix1 = stack[top--];
            matrix_sf *matrix2 = stack[top--];
            matrix_sf *result = mult_mats_sf(matrix2, matrix1);
            result->name = temp_name++;

            if(!isalpha(matrix1->name)){
                free(matrix1);
            }
            if(!isalpha(matrix2->name)){
                free(matrix2);
            }

            stack[++top] = result;
        }
    }

    matrix_sf *result = stack[top];
    result->name = name;
    free(stack);
    free(postfix);
    return result;
}

matrix_sf *execute_script_sf(char *filename) {
    FILE *file = fopen(filename, "r");
    char *line = NULL;
    size_t max_line_size = MAX_LINE_LEN;
    bst_sf *root = NULL;
    matrix_sf *last_matrix = NULL;

    while(getline(&line, &max_line_size, file) != -1){
        char name = line[0];
        char *expr = strchr(line, '=') + 1;

        while (isspace(*expr)){
            expr++;
        }

        matrix_sf *matrix = NULL;

        if(isdigit(*expr)){
            matrix = create_matrix_sf(name, expr);
        }
        else {
            matrix = evaluate_expr_sf(name, expr, root);
        }

        root = insert_bst_sf(matrix, root);
        last_matrix = matrix;
    }

    matrix_sf *ans = copy_matrix(last_matrix->num_rows, last_matrix->num_cols, last_matrix->values);
    free(line);
    free_bst_sf(root);
    fclose(file);
    return ans;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
