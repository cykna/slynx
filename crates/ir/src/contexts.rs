use slynx_hir::{HirStatement, VariableId};

use crate::{Context, IRError, IRPointer, SlynxIR, TempIRData};

impl SlynxIR {
    pub(crate) fn initialize_function(
        &mut self,
        ir: IRPointer<Context, 1>,
        statements: &[HirStatement],
        args: &[VariableId],
        temp: &mut TempIRData,
    ) -> Result<(), IRError> {
        temp.set_current_function(ir);

        {
            let label = self.insert_label(ir, "entry");
            self.get_context_mut(ir).set_label_ptr(label.with_length()); //must do so because this gets the next avaible position to labels
            let next_instruction = self.get_next_mapeable_instruction_ptr();
            let mut ptr = next_instruction.with_length();
            ptr.set_length(ptr.len() - 1);
            self.get_label_mut(label).set_instructions_pointer(ptr);
            temp.set_current_label(label);
        }

        let ptr = IRPointer::new(self.values.len(), args.len());
        for (idx, _) in args.iter().enumerate() {
            self.insert_value(self.create_func_arg_value(idx, temp));
        }
        temp.set_function_args(args, ptr);

        for statement in statements {
            self.generate_statement(statement, temp)?;
        }

        Ok(())
    }
}
