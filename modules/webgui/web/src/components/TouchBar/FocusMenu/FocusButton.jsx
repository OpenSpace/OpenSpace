import React, { Component } from 'react';
import PropTypes from 'prop-types';

import DataManager from '../../../api/DataManager';
import { OriginKey } from '../../../api/keys';
import { jsonToLuaString } from '../../../utils/propertyTreeHelpers';
import styles from './FocusMenu.scss';

class FocusButton extends Component {
  constructor(props) {
    super(props);
    this.select = this.select.bind(this);
  }

  get isActive() {
    return this.props.name === this.props.active;
  }

  select() {
    DataManager.setValue(OriginKey, jsonToLuaString(this.props.name));
  }

  render() {
    return (
      <button className={`${styles.button} ${this.isActive && styles.active}`} onClick={this.select}>
        {this.props.name}
      </button>
    );
  }
}

FocusButton.propTypes = {
  active: PropTypes.string.isRequired,
  name: PropTypes.string.isRequired,
};

export default FocusButton;
