import React, { Component } from 'react';
import PropTypes from 'prop-types';

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
    this.props.onChangeOrigin(this.props.name);
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
  onChangeOrigin: PropTypes.func.isRequired,
};

export default FocusButton;
