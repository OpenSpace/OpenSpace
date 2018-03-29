// @flow
import React, { Element } from 'react';

import styles from './Overlay.scss';

const Overlay = (props: { children: Element<any> }): Element<any> => (
  <div className={styles.messageOverlay}>
    { props.children }
  </div>
);

export default Overlay;
