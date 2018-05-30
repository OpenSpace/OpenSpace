import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { connect } from 'react-redux';
import Proptypes from 'prop-types'; 
import styles from './DataLoader.scss';
import Window from '../common/Window/Window';
import { toggleActivated } from '../../api/Actions/dataLoaderActions';


class DataLoader extends Component {
  constructor(props) {
    super(props);

  }

  render() {
    const {toggleActivated, activated } = this.props
    return(
      <div id="page-content-wrapper">
        { this.props.activated && (
          <div className={styles.center-content}>
            <Window
              title="Data Loader"
              size={{ width:600, height:400 }}
              position={{ x:470, y:-370 }}
              closeCallback={() => toggleActivated(!activated)}
            >
              {/* Array-list to be read from. Buttons selecting what data type to load */}
            </Window>
          </div>
        )}
      </div>
    );
  }
}

const mapStateToProps = state => ({
  activated: state.dataLoader.activated
});

const mapDispatchToProps = dispatch => ({
  toggleActivated: (activated) => {
    dispatch(toggleActivated(activated));
  }
});

DataLoader = connect(
  mapStateToProps,
  mapDispatchToProps
)(DataLoader);

export default DataLoader;