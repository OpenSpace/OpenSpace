import { applyMiddleware } from 'redux';
import { updateBackend } from './propertyTree';
import { connection } from './connection';
// import logger from './logger';

const middleware = applyMiddleware(
  // logger, //middleWare for logging state change
  updateBackend,
  connection,
);

export default middleware;
