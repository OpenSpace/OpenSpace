import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Icon from '../../common/Icon/Icon';
import styles from './MarkerInfo.scss';
import Popover from '../../common/Popover/Popover';

class MarkerInfoIcon extends Component {
  constructor(props) {
    super(props);
    this.state = {
      showInfoWindow: false,
    };
  }

  componentWillUnmount() {
    this.setState({ showInfoWindow: false });
  }

  toggleInfoWindow() {
    this.setState({
      showInfoWindow: !this.state.showInfoWindow,
    });
  }

  render() {
    const { positionStyles, identifier, planetInfo } = this.props;
    return (
      <div>
        <Icon
          onClick={() => this.toggleInfoWindow()}
          className={styles.Icon}
          icon="info_outline"
          style={positionStyles.Icon}
        />
        {this.state.showInfoWindow &&
        <Popover
          className={styles.InfoPopover}
          arrow=""
          title={identifier}
          closeCallback={() => this.toggleInfoWindow()}
        >
          <p className={styles.InfoText}>
            {planetInfo ? planetInfo.info : 'No data available'}
          </p>
        </Popover>}
      </div>);
  }
}

MarkerInfoIcon.PropTypes = {
  positionStyles: PropTypes.objectOf(PropTypes.shape({})).isRequired,
  identifier: PropTypes.string.isRequired,
  planetInfo: PropTypes.objectOf(PropTypes.shape({})).isRequired,
};

export default MarkerInfoIcon;
