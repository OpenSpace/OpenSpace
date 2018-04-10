import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Icon from '../../common/Icon/Icon';
import SmallLabel from '../../common/SmallLabel/SmallLabel';
import styles from './FocusButton.scss';

class OverViewButton extends Component {
  constructor(props) {
    super(props);
    this.select = this.select.bind(this);
  }

  get isActive() {
    return this.props.value;
  } 

  select() {
    this.props.onChangeView(this.props.value ? '0' : '1');
  }

  render() {
    return (
      <div className={`${styles.FocusButton} ${this.isActive && styles.active}`} onClick={this.select} role="button" tabIndex="0">
        <Icon icon="track_changes" className={styles.Icon} />
        <SmallLabel>Overview</SmallLabel>
      </div>
    );
  }
}

OverViewButton.propTypes = {
  value: PropTypes.bool.isRequired,
  onChangeView: PropTypes.func.isRequired,
};

export default OverViewButton;
