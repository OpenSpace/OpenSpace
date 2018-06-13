import React, { Component } from 'react';
import PropTypes from 'prop-types'; 
import styles from './DataItemList.scss';

import DataManager from '../../../api/DataManager';

import { getDirectoryLeaf } from '../utils/helpers';

const handleClick = (dirLeaf) => {
  DataManager.trigger(`Modules.DataLoader.Loader.ItemTrigger_${dirLeaf}`);
}

// TODO: class
const DataItemList = (props) => (
  <div className={styles.list}>
    {props.items.length > 0 && props.items.map(p => {
      const dirLeaf = getDirectoryLeaf(p);
       
      return (
      <div className={styles.item}
           key={p}
           onClick={() => handleClick(dirLeaf)}>
        {dirLeaf}
      </div>
      )}
    )}
  </div>
)

DataItemList.propTypes = {
  items: PropTypes.arrayOf(PropTypes.string)
};

DataItemList.defaultProps = {
  items: [],
};


export default DataItemList;