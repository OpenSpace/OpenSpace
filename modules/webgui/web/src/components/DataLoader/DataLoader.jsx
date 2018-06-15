import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { connect } from 'react-redux';
import Proptypes from 'prop-types'; 

import DataItemList from './presentational/DataItemList';
import { stringListToArray } from './utils/helpers';

import DataManager from '../../api/DataManager';
import styles from './DataLoader';
import Window from '../common/Window/Window';
import { setActivated } from '../../api/Actions/dataLoaderActions';
import Button from '../common/Input/Button/Button';
import Label from '../common/Label/Label';
import UploadDataButton from './UploadDataButton';
import provideWindowWidth from './HOC/provideWindowSize';

class DataLoader extends Component {
  constructor(props) {
    super(props);

    this.dataTypesToLoad = ['Volumes', 'Fieldlines'];

    this.handleDataTypeList = this.handleDataTypeList.bind(this);

    this.state = {
      activeDataType: '',
      dataToLoadUri: '',
      dataItems: [],
      width: 900,
      height: 500
    };
  }

  shouldComponentUpdate(nextProps, nextState) {
    const { activeDataType, dataToLoadUri, width } = this.state;
    if ((activeDataType !== nextState.activeDataType) && (nextState.activeDataType !== '')) {
      this.triggerDataToLoad(nextState.activeDataType);
      const uri = this.getUriForDataToLoad(nextState.activeDataType);
      this.setState({ dataToLoadUri: uri }, this.subscribeToActiveUri(uri));
    }

    if (dataToLoadUri !== nextState.dataToLoadUri) {
      this.subscribeToActiveUri(nextState.dataToLoadUri);
    }

    if (width !== nextProps.width && nextProps.width !== 0 ) {
      this.setState({
        width: nextProps.width / 2,
        height: nextProps.height / 2
      })
    }

    return true;
  }

  getUriForDataToLoad(dataType) {
    let uri = 'Modules.DataLoader.';

    for (const type of this.dataTypesToLoad) {
      if (dataType == type) {
        uri += type;
      }
    }

    return uri;
  }

  triggerDataToLoad(dataType) {
    DataManager.trigger(`Modules.DataLoader.ShowInternal${dataType}Trigger`)
  }

  handleDataTypeList(data) {
    this.setState({dataItems: stringListToArray(data.Value)});
  }

  subscribeToActiveUri(uri = '') {
    DataManager.subscribe(uri || this.state.dataToLoadUri, this.handleDataTypeList);
  }

  render() {
    const {setActivated, activated } = this.props

    let DataTypeButtons = () => {
      return(
        <section className={styles.dataButtons}>
          <div>
            {this.dataTypesToLoad.map((dataType) => 
              <Button 
                key={dataType} 
                onClick={() => this.setState({activeDataType: dataType})}
                disabled={dataType == 'Fieldlines'}>
                <Label>{dataType}</Label>
              </Button>
            )}
          </div>
        </section>
      );
    };

    return(
      <div className="page-content-wrapper">
        { this.props.activated && (
          <div className={styles.centerContent}>
            <Window
              type="large"
              title="Data Loader"
              size={{ width:this.state.width, height:this.state.height }}
              position={{ x:470, y:-370 }}
              closeCallback={() => setActivated(false)}
            >
              <DataTypeButtons/>
              <DataItemList items={this.state.dataItems} />
              <UploadDataButton/>
            </Window>
          </div>
        )}
      </div>
    );
  }
}

const mapStateToProps = state => ({
  activated: state.dataLoader.activated,
  filePaths: state.dataLoader.filePaths,
});

const mapDispatchToProps = dispatch => ({
  setActivated: (isActivated) => {
    dispatch(setActivated(isActivated));
  },
});

DataLoader = connect(
  mapStateToProps,
  mapDispatchToProps
)(DataLoader);

export default provideWindowWidth(DataLoader);