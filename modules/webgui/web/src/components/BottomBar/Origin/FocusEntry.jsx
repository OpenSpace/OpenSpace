import React, { Component } from 'react';
import PropTypes from 'prop-types';
import DataManager from '../../../api/DataManager';
import styles from './FocusEntry.scss';

const ORIGIN_KEY = 'NavigationHandler.Origin';

class FocusEntry extends Component {
  constructor(props) {
    super(props);
    this.select = this.select.bind(this);
  }

  select() {
    const { name } = this.props;
    if (this.props.onClick) {
      this.props.onClick(name);
    } else {
      DataManager.setValue(ORIGIN_KEY, name);
    }
  }

  get isActive() {
    return this.props.name === this.props.active;
  }

  render() {
    const { name } = this.props;
    return (
      <li className={`${styles.entry} ${this.isActive && styles.active}`} onClick={this.select}>
        <span className={styles.title}>
          { name }
        </span>
      </li>
    );
  }
}

FocusEntry.propTypes = {
  name: PropTypes.string.isRequired,
  onClick: PropTypes.func,
  active: PropTypes.string,
};

FocusEntry.defaultProps = {
  onClick: null,
  active: '',
};

export default FocusEntry;
