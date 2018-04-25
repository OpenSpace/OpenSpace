import React, {Component} from 'react';
import Icon from "../../common/Icon/Icon";
import styles from './MarkerInfo.scss';
import Popover from "../../common/Popover/Popover";

class MakerInfoIcon extends Component {
  constructor(props){
    super(props);
    this.state = {
      showInfoWindow: false,
    };
  }

  toggleInfoWindow(){
    this.setState({
      showInfoWindow: !this.state.showInfoWindow,
    });
  }

  componentWillUnmount(){
    this.setState({showInfoWindow: false})
  }

  render(){
    const { positionStyles, identifier, planetInfo } = this.props;
    return (
      <div>
        <Icon
          onClick={() => this.toggleInfoWindow()}
          className={styles.icon}
          icon="info_outline" style={positionStyles.Icon}
        />
        {this.state.showInfoWindow &&
        <Popover
          style={{position:'absolute', transform: 'translate(120%, -50%)'}}
          arrow=""
          title={identifier}
          closeCallback={() => this.toggleInfoWindow()}>
          {planetInfo ? planetInfo.info : 'No data available'}
        </Popover>}
      </div>)
  }
}

export default MakerInfoIcon;
