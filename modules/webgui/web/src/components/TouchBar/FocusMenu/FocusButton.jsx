import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Icon from '../../common/Icon/Icon';
import SmallLabel from '../../common/SmallLabel/SmallLabel';
import styles from './FocusButton.scss';

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
      <div className={`${styles.FocusButton} ${this.isActive && styles.active}`} onClick={this.select} role="button" tabIndex="0">
        <Icon icon="language" className={styles.Icon} />
        <SmallLabel>{this.props.name}</SmallLabel>
      </div>
    );
  }
}

FocusButton.propTypes = {
  active: PropTypes.string.isRequired,
  name: PropTypes.string.isRequired,
  onChangeOrigin: PropTypes.func.isRequired,
};

export default FocusButton;
