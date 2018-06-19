import React, { Component } from 'react';
import PropTypes from 'prop-types'; 
import styles from './DataItemList.scss';

import DataManager from '../../../api/DataManager';

import { getDirectoryLeaf } from '../utils/helpers';

const LoadingItemsString = 'Loading items...';
const NoItemsFoundString = 'No items found.';

class DataItemList extends React.Component {
  constructor(props) {
    super(props);

    this.handleClick = this.handleClick.bind(this);
  }

  handleClick(path) {
    // DataManager.trigger(`Modules.DataLoader.Loader.ItemTrigger_${dirLeaf}`);
    DataManager.loadDataItem(path);
  }

  render() {
    const { items } = this.props;
    const noItems = items[0] === LoadingItemsString || items[0] === NoItemsFoundString;

    return (
      <div className={styles.list}>
        {items.map(itemPathString => (
          <div className={noItems ? styles.nonItem : styles.item}
              key={itemPathString}
              onClick={noItems ? () => {} : () => this.handleClick(itemPathString)}>
            {noItems ? itemPathString : getDirectoryLeaf(itemPathString)}
          </div>
          )
        )}
      </div>
    )

  }
}

DataItemList.propTypes = {
  items: PropTypes.arrayOf(PropTypes.string)
};

DataItemList.defaultProps = {
  items: [LoadingItemsString],
};


export default DataItemList;