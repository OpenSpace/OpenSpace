import { applyMiddleware } from 'redux';
import { updateBackend } from './propertyTree';
import { connection } from './connection';
import { fetchData } from './fetchData';
// import logger from './logger';

const middleware = applyMiddleware(
  // logger, //middleWare for logging state change
  updateBackend,
  connection,
  fetchData,
);

export default middleware;
