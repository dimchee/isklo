import init, { simplify, explain, egraph } from './www/wasm.js';

onmessage = async (e) => { 
  await init({});
  try {
    postMessage(explain(e.data[0], e.data[1])); 
  } catch (e) {
    console.log("egg-wasm error: ", e.toString())
  }
}
