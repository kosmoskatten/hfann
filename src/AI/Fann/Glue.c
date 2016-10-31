
#include "fannwrap.h"

FannRec * inline_c_AI_Fann_Glue_0_dba20a4c9a759c80ccf3a307b39ca4d2967976b7(unsigned input_inline_c_0, unsigned hidden_inline_c_1, unsigned output_inline_c_2) {

        return fann_create_standard(3, input_inline_c_0,
                                       hidden_inline_c_1,
                                       output_inline_c_2);
    
}


void inline_c_AI_Fann_Glue_1_acaaa066e0f96e5aadc5a3606d1ba5ccc72bb4f1(FannRec * ptr_inline_c_0) {

        fann_destroy(ptr_inline_c_0);
    
}


void inline_c_AI_Fann_Glue_2_3fe41fc159b6da4e02e5b0872b801cae6ac974b7(FannRec * ptr_inline_c_0, int val_inline_c_1) {

        fann_set_activation_function_hidden(ptr_inline_c_0,
                                            val_inline_c_1);
    
}


void inline_c_AI_Fann_Glue_3_e8f16d5d23676e26b51c4ad718f703e496f54e77(FannRec * ptr_inline_c_0, int val_inline_c_1) {

        fann_set_activation_function_output(ptr_inline_c_0,
                                            val_inline_c_1);
    
}
