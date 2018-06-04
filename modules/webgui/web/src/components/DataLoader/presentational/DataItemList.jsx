import React, { Component } from 'react';
import Proptypes from 'prop-types'; 
import styles from './DataItemList.scss';
// import Label from '../common/Label/Label';

const getDirectoryLeaf = (directory) => {
  const leaf = directory.match('([^/]+)/?$');
  console.log(leaf);
  // std::regex dirLeaf_regex("([^/]+)/?$");
}

const DataItemList = (props) => (
  <div className={styles.list}>
    {props.items && props.items.map(p => (
      <div class={styles.item}
           onClick={() => console.log(`clicked ${p}`)}>
        {getDirectoryLeaf(p)}
      </div>
    ))}
  </div>
)

// define props

export default DataItemList;