// @flow
import React, { Element } from 'react';

import styles from './DisruptiveMessage.scss';

const DisruptiveMessage = (props: { children: Element<any> }): Element<any> => (
  <div className={styles.messageOverlay}>
    { props.children }
  </div>
);

export default DisruptiveMessage;
