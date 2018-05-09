import React, { Component } from 'react';
import { withRouter } from 'react-router-dom';
import { connect } from 'react-redux';
import '../styles/base.scss';

import Error from '../components/common/Error/Error';
import Overlay from '../components/common/Overlay/Overlay';
import { startConnection } from '../api/Actions';

import styles from './OnStartMenu.scss';
import Slider from '../components/ImageSlider/Slider';

class OnStartMenu extends Component {
  constructor(props) {
    super(props);
  }

  componentDidMount() {
    this.props.StartConnection();
  }

  render() {
    return (
      <div className={styles.app}>
        { this.props.connectionLost && (
          <Overlay>
            <Error>
              Connection lost. Trying to reconnect again soon.
            </Error>
          </Overlay>
        )}
        <Slider/>
      </div>
    );
  }
}

const mapStateToProps = state => ({
    connectionLost: state.connection.connectionLost,
});

const mapDispatchToProps = dispatch => ({
  StartConnection: () => {
    dispatch(startConnection());
  },
});

OnStartMenu = withRouter(connect(
  mapStateToProps,
  mapDispatchToProps,
)(OnStartMenu));

export default OnStartMenu;
