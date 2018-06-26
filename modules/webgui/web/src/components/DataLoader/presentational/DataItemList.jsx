import React, { Component } from 'react';
import PropTypes from 'prop-types'; 
import styles from './DataItemList.scss';

import DataManager from '../../../api/DataManager';
import { ValuePlaceholder, LoadDataItemScript } from '../../../api/keys';

import { getDirectoryLeaf } from '../utils/helpers';

const LoadingItemsString = 'Loading items...';
const NoItemsFoundString = 'No items found.';

const Item = (props) => (
  <div className={props.classes}
       onClick={props.onClick}>
    {props.children}
  </div>
)

class DataItemList extends React.Component {
  constructor(props) {
    super(props);

    this.handleClick = this.handleClick.bind(this);
  }

  handleClick(path) {
    const script = LoadDataItemScript.replace(ValuePlaceholder, `\'${path}\'`);
    DataManager.runScript(script);
  }

  render() {
    const { items } = this.props;
    const hasItems = items.length > 0;

    return (
      <div className={styles.list}>
        {hasItems && items.map((itemPathString, i) => {
          let classes = styles.item;
          classes += (i+1) % 2 === 0 ? ' ' + styles.even : '';

          return (
            <Item classes={classes}
                  key={itemPathString}
                  onClick={() => this.handleClick(itemPathString)}>
              {getDirectoryLeaf(itemPathString)}
            </Item>
          )}
        )}

        {!hasItems ? (
          <Item classes={styles.nonItem}
                key={NoItemsFoundString}
                onClick={() => {}}>
            {NoItemsFoundString}
          </Item>
        ) : null}
      </div>
    )

  }
}

DataItemList.propTypes = {
  items: PropTypes.arrayOf(PropTypes.string),
};

DataItemList.defaultProps = {
  items: []
};

export default DataItemList;