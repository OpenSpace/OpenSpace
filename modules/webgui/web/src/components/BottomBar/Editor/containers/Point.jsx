import React, {Component} from 'react'
import { connect } from 'react-redux';
import { toggleActiveEnvelope, toggleActivePoint} from '../actions';
import WTF from '../presentational/Point.jsx'
import PropTypes from 'prop-types';
import styles from '../style/Point.scss';

class Point extends Component{
  constructor(props) {
    super(props);

    this.state = {
      alteredPosition: {
        x: this.props.position.x,
        y: this.props.position.y,
      },
      clickable: true,
    }
    this.handleDrag = this.handleDrag.bind(this);
    this.handleClick = this.handleClick.bind(this);
  }

  handleDrag(e, ui) {
    const {alteredPosition, actualPosition} = this.state;
    this.setState({
      clickable: false,
    });

    let position = {
        x: this.props.position.x + ui.deltaX,
        y: this.props.position.y,
      }

    if(!this.props.anchor)
      position.y = position.y + ui.deltaY;
    this.props.handleOnDrag(position, this.props.id, this.props.envelope.id);
  }

  hasActiveSibling(envelope) {
    var hasActiveSibling = false;
    envelope.points.forEach(function(point) {
      if (point.active)
        hasActiveSibling = true;
    })
    return hasActiveSibling;
  }

  handleClick() {
    if (this.state.clickable === false) {
      this.setState({
        clickable: true
      })
    }
    else {
      const {id, active, envelope} = this.props;
      if (envelope.active === true) {
        this.props.TogglePoint(this.props.envelope.id, this.props.id);
      }
      else if(this.hasActiveSibling(envelope)) {
        this.props.TogglePoint(this.props.envelope.id, this.props.id);
      }
      else {
        this.props.ToggleEnvelope(this.props.envelope.id);
      }
    }
  }

  render() {
    return(
    <WTF
      handleClick={() => this.handleClick()}
      handleDrag={(e, ui) => this.handleDrag(e, ui)}
      height={this.props.height}
      width={this.props.width}
      color={this.props.color}
      active={(this.props.active || this.props.envelope.active) ? true : false}
      anchor={this.props.anchor}
      position={this.state.alteredPosition}
    />
    );
  }
}
Point.propTypes = {
  handleOnDrag: PropTypes.func.isRequired,
  position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y:PropTypes.number.isRequired,
            }).isRequired,
  anchor: PropTypes.bool.isRequired,
  color: PropTypes.string.isRequired,
  id: PropTypes.number.isRequired,
  active: PropTypes.bool.isRequired,
}

const mapDispatchToProps = (dispatch) => {
  return {
    ToggleEnvelope: (id) => {
      dispatch(toggleActiveEnvelope(id));
    },
    TogglePoint: (envelopeId, pointId) => {
      dispatch(toggleActivePoint(envelopeId, pointId));
    },

  }
}

Point = connect(
  null,
  mapDispatchToProps,
  )(Point)

export default Point;
