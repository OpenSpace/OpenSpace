import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { connect } from 'react-redux';
import Proptypes from 'prop-types'; 
import styles from './DataLoader.scss';
import Window from '../common/Window/Window';
import { setActivated, setFilePaths } from '../../api/Actions/dataLoaderActions';
import Button from '../common/Input/Button/Button';
import SmallLabel from '../common/SmallLabel/SmallLabel';

class DataLoader extends Component {
  constructor(props) {
    super(props);

    this.state={activeDataType: ''};
  }

  // handleChange(event) {
  //   let filePathString = event.target.value;
  // };

  render() {
    const {setActivated, activated } = this.props
    let buttonArray = ["Volume", "Fieldlines"];

    let dataTypeButtons = () => {
      return(
        <section className={styles.dataButtons}>
          <SmallLabel>
            Select data type you wish to load
          </SmallLabel>
          <div>
            {buttonArray.map((element) => 
              <Button 
                key={element} 
                onClick={() => this.setState({activeDataType: element})}>
                <SmallLabel>{element}</SmallLabel>
              </Button>
            )}
          </div>
        </section>
      );
    };

    let uploadDataButton = () => {
      return(
        <div>
          <label>
            <input 
              type="file"
              style={{opacity: 0}} 
              // onChange={(event) => this.handleChange(event)}
              // accept=".cdf, .osfls"/>
              multiple
              />    
            Upload Data
            {/* Style above - Or replace with <Button/> */}
          </label>
        </div>
      );
    };

    return(
      <div id="page-content-wrapper">
        { this.props.activated && (
          <div className={styles.center-content}>
            <Window
              title="Data Loader"
              // Temporary position and size fix
              size={{ width:600, height:400 }}
              position={{ x:470, y:-370 }}
              closeCallback={() => setActivated(false)}
            >
              { dataTypeButtons() }
              { uploadDataButton() }
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
  setFilePaths: (filePaths) => {
    dispatch(setFilePaths(filePaths))
  },

  setActivated: (isActivated) => {
    dispatch(setActivated(isActivated));
  },
});

DataLoader = connect(
  mapStateToProps,
  mapDispatchToProps
)(DataLoader);

export default DataLoader;