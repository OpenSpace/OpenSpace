import React, { Component } from 'react';
import { withRouter } from 'react-router-dom';
import { connect } from 'react-redux';
import '../styles/base.scss';

import Error from '../components/common/Error/Error';
import Overlay from '../components/common/Overlay/Overlay';
import { startConnection } from '../api/Actions';
import TouchBar from '../components/TouchBar/TouchBar';

class OnTouchGui extends Component {
  componentDidMount() {
    this.props.StartConnection();
  }

  render() {
    return (
      <div className="OnTouchGui">
        { this.props.connectionLost && (
          <Overlay>
            <Error>
              Connection lost. Trying to reconnect again soon.
            </Error>
          </Overlay>
        )}
        <TouchBar />
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

OnTouchGui = withRouter(connect(
  mapStateToProps,
  mapDispatchToProps,
)(OnTouchGui));

export default OnTouchGui;
