const MAX_COMMAND_IN_QUEUE = 4096;

export const CommandBuffer = {
  index: 0,
  funcs: [] as Function[],
  targets: new Uint16Array(MAX_COMMAND_IN_QUEUE),
  params: [] as any[],
  param_lengths: new Uint8Array(MAX_COMMAND_IN_QUEUE),
  push(cmd: Function, target: number, params: any[]) {
    CommandBuffer.funcs[this.index] = cmd;
    CommandBuffer.targets[this.index] = target;
    {
      let offset = this.params.length;
      for (let k = 0, j = params.length; k < j;) {
        CommandBuffer.params[offset + k++] = params[k];
      }
    }
    CommandBuffer.param_lengths[this.index++] = params.length;
  },
};

function queue_executor() {
  let params_index = 0;
  const params = [];
  for (
    let i = 0, j = CommandBuffer.param_lengths[0];
    i < CommandBuffer.index;
    j = CommandBuffer.param_lengths[i++]
  ) {
    const func = CommandBuffer.funcs[i];
    params[0] = CommandBuffer.targets[i];
    for (let k = 1; k <= j; k++) {
      params[k] = CommandBuffer.params[params_index++];
    }
    func.apply(undefined, params);
  }
  CommandBuffer.params.length = 0;
  CommandBuffer.index = 0;
}

///Requests the execution of the command buffer on redrawing. 
export function requestRerender() {
  requestAnimationFrame(queue_executor);
}
