import React, { Component } from 'react';
import { withRouter } from 'react-router-dom';
import { connect } from 'react-redux';
import '../styles/base.scss';

import Error from '../components/common/Error/Error';
import Overlay from '../components/common/Overlay/Overlay';
import { changePropertyValue, startConnection, fetchData } from '../api/Actions';
import TouchBar from '../components/TouchBar/TouchBar';
import styles from './OnTouchGui.scss';
import { traverseTreeWithURI } from "../utils/propertyTreeHelpers";
import { infoIconKey, SetGoToGeoScript, StoryKey, ValuePlaceholder } from '../api/keys';
import DataManager from '../api/DataManager';

// TODO Remove and replace with input from API
const startPosition = { location: { lat: '58.5877', long: '16.1924', att: '20000000' } };

class OnTouchGui extends Component {
  constructor(props) {
    super(props);

    this.changeStory = this.changeStory.bind(this);
  }
  componentDidMount() {
    this.props.StartConnection();
    this.props.FetchData(infoIconKey);

    const script = SetGoToGeoScript.replace(ValuePlaceholder, `${startPosition.location.lat}, ${startPosition.location.long}, ${startPosition.location.att}`);
    DataManager.runScript(script);
  }

  changeStory(e){
    this.props.ChangePropertyValue(this.props.storyIdentifierNode.Description, e.target.id);
  }

  render() {
    /* TODO Add buttons for changing story
      <button onClick={this.changeStory} id={'Story.SolarEclipse'}>Solar Eclipse Story</button>
      <button onClick={this.changeStory} id={'Story.Basic'}>Solar System Story</button>
      {this.props.storyIdentifierNode.Value !== 'Story.None' && <TouchBar />}
    */

    return (
      <div className={styles.app}>
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

const mapStateToProps = state => {
  let storyIdentifierNode = [];

if (Object.keys(state.propertyTree).length !== 0) {
  storyIdentifierNode = traverseTreeWithURI(state.propertyTree, StoryKey);
}
return {
  storyIdentifierNode,
  connectionLost: state.connection.connectionLost,
}};

const mapDispatchToProps = dispatch => ({
  ChangePropertyValue: (discription, URI) => {
    dispatch(changePropertyValue(discription, URI));
  },
  StartConnection: () => {
    dispatch(startConnection());
  },
  FetchData: (id) => {
    dispatch(fetchData(id));
  }
});

OnTouchGui = withRouter(connect(
  mapStateToProps,
  mapDispatchToProps,
)(OnTouchGui));

export default OnTouchGui;
