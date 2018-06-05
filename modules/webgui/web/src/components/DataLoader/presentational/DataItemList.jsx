import React, { Component } from 'react';
import PropTypes from 'prop-types'; 
import styles from './DataItemList.scss';

import { getDirectoryLeaf } from '../utils/helpers';

const DataItemList = (props) => (
  <div className={styles.list}>
    {props.items.length > 0 && props.items.map(p => (
      <div className={styles.item}
           key={p}
           onClick={() => console.log(`clicked ${p}`)}>
        {getDirectoryLeaf(p)}
      </div>
    ))}
  </div>
)

DataItemList.propTypes = {
  items: PropTypes.arrayOf(PropTypes.string)
};

DataItemList.defaultProps = {
  items: [],
};


export default DataItemList;